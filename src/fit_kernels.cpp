// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

inline double mills_ratio_scalar(const double u) {
  const double logphi = R::dnorm4(u, 0.0, 1.0, 1);
  double logPhi = R::pnorm5(u, 0.0, 1.0, 1, 1);
  if (logPhi < -745.0) {
    logPhi = -745.0;
  }
  return std::exp(logphi - logPhi);
}

inline double skewnorm_logdens_scalar(
    const double x,
    const double xi,
    const double omega,
    const double alpha,
    const double logC) {
  const double xx = (x - xi) / omega;
  return std::log(2.0) +
    R::dnorm4(xx, 0.0, 1.0, 1) +
    R::pnorm5(alpha * xx, 0.0, 1.0, 1, 1) -
    std::log(omega) +
    logC;
}

inline MatrixXd reconstruct_free_matrix(const List& spec, const NumericVector& x_free) {
  NumericVector values = clone(as<NumericVector>(spec["values"]));
  IntegerVector free_x_idx = spec["free_x_idx"];
  IntegerVector free_pos_idx = spec["free_pos_idx"];
  const int nrow = as<int>(spec["nrow"]);
  const int ncol = as<int>(spec["ncol"]);

  for (int k = 0; k < free_x_idx.size(); ++k) {
    values[free_pos_idx[k] - 1] = x_free[free_x_idx[k] - 1];
  }

  MatrixXd out(nrow, ncol);
  for (int j = 0; j < ncol; ++j) {
    for (int i = 0; i < nrow; ++i) {
      out(i, j) = values[i + j * nrow];
    }
  }
  return out;
}

inline MatrixXd matrix_from_flat(const NumericVector& vals, const int nrow, const int ncol) {
  MatrixXd out(nrow, ncol);
  for (int j = 0; j < ncol; ++j)
    for (int i = 0; i < nrow; ++i)
      out(i, j) = vals[i + j * nrow];
  return out;
}

inline MatrixXd missing_pattern_cov_from_record(const List& rec) {
  NumericVector sample_cov_vals = rec["SY_values"];
  IntegerVector sy_nrow_v = rec["SY_nrow"];
  IntegerVector sy_ncol_v = rec["SY_ncol"];
  IntegerVector p_obs_v = rec["p_obs"];
  const int sy_nrow = sy_nrow_v[0];
  const int sy_ncol = sy_ncol_v[0];
  const int p_obs = p_obs_v[0];

  if (sample_cov_vals.size() == 1 && sy_nrow == 1 && sy_ncol == 1 && p_obs > 1) {
    return MatrixXd::Zero(p_obs, p_obs);
  }

  return matrix_from_flat(sample_cov_vals, sy_nrow, sy_ncol);
}

inline MatrixXd lisrel_implied_cov_group_impl(const List& group, const NumericVector& x_free) {
  List matrices = group["matrices"];
  const bool has_beta = as<bool>(group["has_beta"]);

  const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
  const MatrixXd theta = reconstruct_free_matrix(matrices["theta"], x_free);
  const MatrixXd psi = reconstruct_free_matrix(matrices["psi"], x_free);

  MatrixXd eta_cov = psi;
  if (has_beta) {
    const MatrixXd beta = reconstruct_free_matrix(matrices["beta"], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) {
      return MatrixXd();
    }
    const MatrixXd Ainv = lu.inverse();
    eta_cov = Ainv * psi * Ainv.transpose();
  }

  return lambda * eta_cov * lambda.transpose() + theta;
}

inline MatrixXd lisrel_implied_mean_group_impl(const List& group, const NumericVector& x_free) {
  List matrices = group["matrices"];
  const bool has_beta = as<bool>(group["has_beta"]);
  const bool has_mean = as<bool>(group["has_mean"]);

  const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
  if (!has_mean) {
    return MatrixXd::Zero(lambda.rows(), 1);
  }

  const MatrixXd nu = reconstruct_free_matrix(matrices["nu"], x_free);
  const MatrixXd alpha = reconstruct_free_matrix(matrices["alpha"], x_free);

  if (has_beta) {
    const MatrixXd beta = reconstruct_free_matrix(matrices["beta"], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) {
      return MatrixXd();
    }
    const MatrixXd Ainv = lu.inverse();
    return nu + lambda * Ainv * alpha;
  }

  return nu + lambda * alpha;
}

inline void set_submatrix(MatrixXd& target, const IntegerVector& idx1, const MatrixXd& src) {
  for (int j = 0; j < idx1.size(); ++j) {
    for (int i = 0; i < idx1.size(); ++i) {
      target(idx1[i] - 1, idx1[j] - 1) = src(i, j);
    }
  }
}

inline void set_subvector(VectorXd& target, const IntegerVector& idx1, const MatrixXd& src) {
  for (int i = 0; i < idx1.size(); ++i) {
    target(idx1[i] - 1) = src(i, 0);
  }
}

struct GcpBlockCpp {
  int p = 0;
  std::vector<int> theta_idx0;
  std::vector<int> row_idx0;
  std::vector<int> col_idx0;
  std::vector<int> iLtheta0;
  std::vector<double> d0;
  bool is_dense = false;
};

struct GcpBlockEvalCpp {
  MatrixXd C;
  MatrixXd J;
};

struct PriorMetaCpp {
  std::vector<int> free_id0;
  std::vector<int> trans_type;
  std::vector<int> prior_type;
  std::vector<double> p1;
  std::vector<double> p2;
  std::vector<bool> is_sd_prior;
  std::vector<bool> is_prec_prior;
};

inline std::vector<GcpBlockCpp> extract_gcp_blocks_cpp(const List& model) {
  if (!model.containsElementNamed("gcp_blocks")) return {};
  const List blocks = model["gcp_blocks"];
  std::vector<GcpBlockCpp> out(blocks.size());
  for (int b = 0; b < blocks.size(); ++b) {
    const List blk = blocks[b];
    GcpBlockCpp cur;
    cur.p = as<int>(blk["p"]);
    IntegerVector theta_idx = blk["theta_idx"];
    IntegerVector row_idx = blk["row_idx"];
    IntegerVector col_idx = blk["col_idx"];
    IntegerVector iLtheta = blk["iLtheta"];
    NumericVector d0 = blk["d0"];
    LogicalVector is_dense = blk["is_dense"];
    cur.theta_idx0.reserve(theta_idx.size());
    cur.row_idx0.reserve(row_idx.size());
    cur.col_idx0.reserve(col_idx.size());
    cur.iLtheta0.reserve(iLtheta.size());
    cur.d0.reserve(d0.size());
    for (int i = 0; i < theta_idx.size(); ++i) cur.theta_idx0.push_back(theta_idx[i] - 1);
    for (int i = 0; i < row_idx.size(); ++i) cur.row_idx0.push_back(row_idx[i] - 1);
    for (int i = 0; i < col_idx.size(); ++i) cur.col_idx0.push_back(col_idx[i] - 1);
    for (int i = 0; i < iLtheta.size(); ++i) cur.iLtheta0.push_back(iLtheta[i] - 1);
    for (int i = 0; i < d0.size(); ++i) cur.d0.push_back(d0[i]);
    cur.is_dense = (bool)is_dense[0];
    out[b] = cur;
  }
  return out;
}

inline PriorMetaCpp extract_prior_meta_cpp(const List& model) {
  PriorMetaCpp pm;
  if (!model.containsElementNamed("prior_cpp")) return pm;
  const List pr = model["prior_cpp"];
  IntegerVector free_id = pr["free_id0"];
  IntegerVector trans_type = pr["trans_type"];
  IntegerVector prior_type = pr["prior_type"];
  NumericVector p1 = pr["p1"];
  NumericVector p2 = pr["p2"];
  LogicalVector is_sd = pr["is_sd_prior"];
  LogicalVector is_prec = pr["is_prec_prior"];
  const int n = free_id.size();
  pm.free_id0.resize(n);
  pm.trans_type.resize(n);
  pm.prior_type.resize(n);
  pm.p1.resize(n);
  pm.p2.resize(n);
  pm.is_sd_prior.resize(n);
  pm.is_prec_prior.resize(n);
  for (int i = 0; i < n; ++i) {
    pm.free_id0[i] = free_id[i];
    pm.trans_type[i] = trans_type[i];
    pm.prior_type[i] = prior_type[i];
    pm.p1[i] = p1[i];
    pm.p2[i] = p2[i];
    pm.is_sd_prior[i] = (bool)is_sd[i];
    pm.is_prec_prior[i] = (bool)is_prec[i];
  }
  return pm;
}

inline void extract_cov_idx_cpp(
    const List& model,
    const int n,
    std::vector<int>& cov_var_idx1,
    std::vector<int>& cov_var_idx2
) {
  cov_var_idx1.assign(n, 0);
  cov_var_idx2.assign(n, 0);
  if (!model.containsElementNamed("cov_var_idx1") || !model.containsElementNamed("cov_var_idx2")) return;
  IntegerVector c1 = model["cov_var_idx1"];
  IntegerVector c2 = model["cov_var_idx2"];
  for (int i = 0; i < std::min(n, (int)c1.size()); ++i) cov_var_idx1[i] = c1[i];
  for (int i = 0; i < std::min(n, (int)c2.size()); ++i) cov_var_idx2[i] = c2[i];
}

inline void gcp_fill_chol_cpp(MatrixXd& L) {
  const int p = L.rows();
  if (p <= 2) return;
  for (int j = 1; j < p; ++j) {
    if (j == 1) continue;
    for (int i = j + 1; i < p; ++i) {
      if (L(i, j) != 0.0) continue;
      bool has_fill = false;
      for (int k = 0; k < j; ++k) {
        if (L(i, k) != 0.0 && L(j, k) != 0.0) {
          has_fill = true;
          break;
        }
      }
      if (!has_fill) continue;
      double acc = 0.0;
      for (int k = 0; k < j; ++k) acc += L(i, k) * L(j, k);
      L(i, j) = -acc / L(j, j);
    }
  }
}

inline MatrixXd gcp_corr_from_theta_cpp(const VectorXd& theta_blk, const GcpBlockCpp& blk) {
  MatrixXd L = MatrixXd::Zero(blk.p, blk.p);
  for (int i = 0; i < blk.p; ++i) L(i, i) = blk.d0[i];
  for (int k = 0; k < (int)blk.iLtheta0.size(); ++k) {
    const int pos = blk.iLtheta0[k];
    const int row = pos % blk.p;
    const int col = pos / blk.p;
    L(row, col) = theta_blk[k];
  }
  gcp_fill_chol_cpp(L);
  const MatrixXd Q = L * L.transpose();
  Eigen::LLT<MatrixXd> llt(Q);
  if (llt.info() != Eigen::Success) return MatrixXd();
  const MatrixXd V = llt.solve(MatrixXd::Identity(blk.p, blk.p));
  const VectorXd s = V.diagonal().array().sqrt();
  MatrixXd C = V;
  for (int j = 0; j < blk.p; ++j) {
    for (int i = 0; i < blk.p; ++i) {
      C(i, j) /= (s[i] * s[j]);
    }
  }
  C = 0.5 * (C + C.transpose());
  C.diagonal().setOnes();
  return C;
}

inline MatrixXd gcp_jacobian_dense_cpp(
    const VectorXd& theta_blk,
    const GcpBlockCpp& blk,
    const MatrixXd& C
) {
  MatrixXd L = MatrixXd::Zero(blk.p, blk.p);
  for (int i = 0; i < blk.p; ++i) L(i, i) = blk.d0[i];
  for (int k = 0; k < (int)blk.iLtheta0.size(); ++k) {
    const int pos = blk.iLtheta0[k];
    const int row = pos % blk.p;
    const int col = pos / blk.p;
    L(row, col) = theta_blk[k];
  }

  const MatrixXd Q = L * L.transpose();
  Eigen::LLT<MatrixXd> llt(Q);
  if (llt.info() != Eigen::Success) return MatrixXd();
  const MatrixXd V = llt.solve(MatrixXd::Identity(blk.p, blk.p));
  const VectorXd s = V.diagonal().array().sqrt();
  const MatrixXd VL = V * L;
  const int m = theta_blk.size();
  MatrixXd J = MatrixXd::Zero(m, m);
  for (int k = 0; k < m; ++k) {
    const int i = blk.row_idx0[k];
    const int j = blk.col_idx0[k];
    const VectorXd vi = V.col(i);
    const VectorXd wj = VL.col(j);
    for (int r = 0; r < m; ++r) {
      const int a = blk.row_idx0[r];
      const int b = blk.col_idx0[r];
      const double dVab = -(vi[a] * wj[b] + wj[a] * vi[b]);
      const double dVaa = -2.0 * vi[a] * wj[a];
      const double dVbb = -2.0 * vi[b] * wj[b];
      const double Cab = C(a, b);
      const double term_a = Cab / (2.0 * V(a, a));
      const double term_b = Cab / (2.0 * V(b, b));
      J(r, k) = dVab / (s[a] * s[b]) - term_a * dVaa - term_b * dVbb;
    }
  }
  return J;
}

inline GcpBlockEvalCpp eval_gcp_block_cpp(const VectorXd& theta_blk, const GcpBlockCpp& blk, const bool want_J) {
  GcpBlockEvalCpp out;
  out.C = gcp_corr_from_theta_cpp(theta_blk, blk);
  if (out.C.size() == 0) return out;
  if (!want_J) return out;
  if (blk.is_dense) {
    out.J = gcp_jacobian_dense_cpp(theta_blk, blk, out.C);
  } else {
    const int m = theta_blk.size();
    out.J = MatrixXd::Zero(m, m);
    const double h = 1e-5;
    for (int k = 0; k < m; ++k) {
      VectorXd th_p = theta_blk;
      VectorXd th_m = theta_blk;
      th_p[k] += h;
      th_m[k] -= h;
      const MatrixXd Cp = gcp_corr_from_theta_cpp(th_p, blk);
      const MatrixXd Cm = gcp_corr_from_theta_cpp(th_m, blk);
      if (Cp.size() == 0 || Cm.size() == 0) return GcpBlockEvalCpp();
      for (int r = 0; r < m; ++r) {
        out.J(r, k) =
          (Cp(blk.row_idx0[r], blk.col_idx0[r]) - Cm(blk.row_idx0[r], blk.col_idx0[r])) / (2.0 * h);
      }
    }
  }
  return out;
}

inline bool theta_to_x_gcp_cpp(
    const std::vector<double>& theta,
    const std::vector<int>& transforms,
    const std::vector<int>& cov_var_idx1,
    const std::vector<int>& cov_var_idx2,
    const std::vector<GcpBlockCpp>& gcp_blocks,
    std::vector<double>& x_free,
    std::vector<GcpBlockEvalCpp>* gcp_eval = nullptr
) {
  const int n = theta.size();
  x_free.assign(n, 0.0);
  for (int i = 0; i < n; ++i) {
    switch (transforms[i]) {
      case 1: x_free[i] = std::exp(theta[i]); break;
      case 2:
        if (cov_var_idx1[i] > 0) {
          const double rho = std::tanh(theta[i]);
          const double sd1 = std::sqrt(std::exp(theta[cov_var_idx1[i] - 1]));
          const double sd2 = std::sqrt(std::exp(theta[cov_var_idx2[i] - 1]));
          x_free[i] = rho * sd1 * sd2;
        } else {
          x_free[i] = std::tanh(theta[i]);
        }
        break;
      default:
        x_free[i] = theta[i];
        break;
    }
  }

  if (gcp_eval) gcp_eval->assign(gcp_blocks.size(), GcpBlockEvalCpp());
  for (int b = 0; b < (int)gcp_blocks.size(); ++b) {
    const auto& blk = gcp_blocks[b];
    VectorXd theta_blk(blk.theta_idx0.size());
    for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) theta_blk[k] = theta[blk.theta_idx0[k]];
    GcpBlockEvalCpp ev = eval_gcp_block_cpp(theta_blk, blk, gcp_eval != nullptr);
    if (ev.C.size() == 0) return false;
    for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
      const int idx = blk.theta_idx0[k];
      const double rho = ev.C(blk.row_idx0[k], blk.col_idx0[k]);
      if (cov_var_idx1[idx] > 0) {
        const double sd1 = std::sqrt(std::exp(theta[cov_var_idx1[idx] - 1]));
        const double sd2 = std::sqrt(std::exp(theta[cov_var_idx2[idx] - 1]));
        x_free[idx] = rho * sd1 * sd2;
      } else {
        x_free[idx] = rho;
      }
    }
    if (gcp_eval) (*gcp_eval)[b] = ev;
  }
  return true;
}

inline double prior_point_logdens_cpp(
    const double x_val,
    const int prior_type,
    const double p1,
    const double p2,
    const bool is_sd,
    const bool is_prec,
    double& dlp_dx,
    double& jac_extra,
    double& log_jac,
    const double dx_dth,
    const double d2x_dth2
) {
  dlp_dx = 0.0;
  jac_extra = 0.0;
  log_jac = 0.0;
  double lp = 0.0;

  if (is_prec) {
    const double prec = 1.0 / x_val;
    double raw_dlp_dprec = 0.0;
    if (prior_type == 1) {
      lp += R::dnorm4(prec, p1, p2, 1);
      raw_dlp_dprec = -(prec - p1) / (p2 * p2);
    } else if (prior_type == 2) {
      lp += R::dgamma(prec, p1, 1.0 / p2, 1);
      raw_dlp_dprec = (p1 - 1.0) / prec - p2;
    } else if (prior_type == 3) {
      const double y = (prec + 1.0) / 2.0;
      lp += R::dbeta(y, p1, p2, 1) - 0.6931471805599453;
      raw_dlp_dprec = (p1 - 1.0) / (prec + 1.0) - (p2 - 1.0) / (1.0 - prec);
    }
    dlp_dx = raw_dlp_dprec * (-1.0 / (x_val * x_val));
    jac_extra = -2.0 * dx_dth / x_val;
    log_jac = std::log(std::abs(dx_dth / (x_val * x_val)));
    return lp + log_jac;
  }

  if (is_sd) {
    const double sd_val = std::sqrt(x_val);
    lp += R::dgamma(sd_val, p1, 1.0 / p2, 1);
    const double dlp_ds = (p1 - 1.0) / sd_val - p2;
    dlp_dx = dlp_ds / (2.0 * sd_val);
    jac_extra = -dx_dth / (2.0 * x_val);
    log_jac = std::log(std::abs(dx_dth / (2.0 * sd_val)));
    return lp + log_jac;
  }

  if (prior_type == 1) {
    lp += R::dnorm4(x_val, p1, p2, 1);
    dlp_dx = -(x_val - p1) / (p2 * p2);
  } else if (prior_type == 2) {
    lp += R::dgamma(x_val, p1, 1.0 / p2, 1);
    dlp_dx = (p1 - 1.0) / x_val - p2;
  } else if (prior_type == 3) {
    const double y = (x_val + 1.0) / 2.0;
    lp += R::dbeta(y, p1, p2, 1) - 0.6931471805599453;
    dlp_dx = (p1 - 1.0) / (x_val + 1.0) - (p2 - 1.0) / (1.0 - x_val);
  }
  log_jac = std::log(std::abs(dx_dth));
  return lp + log_jac;
}

inline bool prior_lp_grad_cpp(
    const std::vector<double>& theta,
    const PriorMetaCpp& pm,
    const std::vector<GcpBlockCpp>& gcp_blocks,
    double& lp_out,
    std::vector<double>* grad_out = nullptr,
    const std::vector<GcpBlockEvalCpp>* gcp_eval_in = nullptr
) {
  thread_local static std::vector<VectorXd> gcp_cache_theta;
  thread_local static std::vector<VectorXd> gcp_cache_gradlogdet;
  if ((int)gcp_cache_theta.size() != (int)gcp_blocks.size()) {
    gcp_cache_theta.assign(gcp_blocks.size(), VectorXd());
    gcp_cache_gradlogdet.assign(gcp_blocks.size(), VectorXd());
  }

  lp_out = 0.0;
  if (grad_out) grad_out->assign(theta.size(), 0.0);
  if (pm.free_id0.empty()) return true;

  std::vector<int> gcp_prior_idx;
  for (int i = 0; i < (int)pm.free_id0.size(); ++i) {
    if (pm.trans_type[i] == 3) gcp_prior_idx.push_back(i);
  }

  for (int i = 0; i < (int)pm.free_id0.size(); ++i) {
    if (pm.trans_type[i] == 3) continue;
    const int fid = pm.free_id0[i];
    const double th = theta[fid];
    double x_val = th, dx_dth = 1.0, d2x_dth2 = 0.0;
    if (pm.trans_type[i] == 1) {
      x_val = std::exp(th);
      dx_dth = x_val;
      d2x_dth2 = x_val;
    } else if (pm.trans_type[i] == 2) {
      const double safe = 1.0 - 1e-6;
      const double tv = std::tanh(th);
      x_val = safe * tv;
      dx_dth = safe * (1.0 - tv * tv);
      d2x_dth2 = safe * (-2.0 * tv * (1.0 - tv * tv));
    }
    double dlp_dx, jac_extra, log_jac;
    const double lp = prior_point_logdens_cpp(
      x_val, pm.prior_type[i], pm.p1[i], pm.p2[i],
      pm.is_sd_prior[i], pm.is_prec_prior[i],
      dlp_dx, jac_extra, log_jac, dx_dth, d2x_dth2
    );
    lp_out += lp;
    if (grad_out) {
      (*grad_out)[fid] += dlp_dx * dx_dth + d2x_dth2 / dx_dth + jac_extra;
    }
  }

  if (gcp_prior_idx.empty() || gcp_blocks.empty()) return true;

  for (int blk_id = 0; blk_id < (int)gcp_blocks.size(); ++blk_id) {
    const auto& blk = gcp_blocks[blk_id];
    std::vector<int> prior_pos;
    for (int i : gcp_prior_idx) {
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        if (pm.free_id0[i] == blk.theta_idx0[k]) {
          prior_pos.push_back(i);
          break;
        }
      }
    }
    if (prior_pos.empty()) continue;

    VectorXd theta_blk(blk.theta_idx0.size());
    for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) theta_blk[k] = theta[blk.theta_idx0[k]];
    GcpBlockEvalCpp ev = (gcp_eval_in && (int)gcp_eval_in->size() > blk_id) ?
      (*gcp_eval_in)[blk_id] :
      eval_gcp_block_cpp(theta_blk, blk, grad_out != nullptr);
    if (ev.C.size() == 0) return false;

    VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
    for (int i : prior_pos) {
      int local_k = -1;
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        if (pm.free_id0[i] == blk.theta_idx0[k]) {
          local_k = k;
          break;
        }
      }
      if (local_k < 0) continue;
      const double rho = ev.C(blk.row_idx0[local_k], blk.col_idx0[local_k]);
      double dlp_dx = 0.0, jac_extra = 0.0, log_jac = 0.0;
      const double lp = prior_point_logdens_cpp(
        rho, pm.prior_type[i], pm.p1[i], pm.p2[i],
        pm.is_sd_prior[i], pm.is_prec_prior[i],
        dlp_dx, jac_extra, log_jac, 1.0, 0.0
      );
      lp_out += lp - log_jac;
      rho_grad[local_k] += dlp_dx;
    }

    const double detJ = ev.J.determinant();
    lp_out += std::log(std::abs(detJ));

    if (grad_out) {
      const VectorXd theta_grad = ev.J.transpose() * rho_grad;
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        (*grad_out)[blk.theta_idx0[k]] += theta_grad[k];
      }
      bool use_cache =
        gcp_cache_theta[blk_id].size() == theta_blk.size() &&
        (gcp_cache_theta[blk_id] - theta_blk).cwiseAbs().maxCoeff() < 1e-4;
      VectorXd grad_logdet;
      if (use_cache) {
        grad_logdet = gcp_cache_gradlogdet[blk_id];
      } else {
        const double h = 1e-5;
        grad_logdet = VectorXd::Zero(blk.theta_idx0.size());
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
          VectorXd th_p = theta_blk, th_m = theta_blk;
          th_p[k] += h;
          th_m[k] -= h;
          GcpBlockEvalCpp ev_p = eval_gcp_block_cpp(th_p, blk, true);
          GcpBlockEvalCpp ev_m = eval_gcp_block_cpp(th_m, blk, true);
          if (ev_p.J.size() == 0 || ev_m.J.size() == 0) return false;
          grad_logdet[k] = (
            std::log(std::abs(ev_p.J.determinant())) -
            std::log(std::abs(ev_m.J.determinant()))
          ) / (2.0 * h);
        }
        gcp_cache_theta[blk_id] = theta_blk;
        gcp_cache_gradlogdet[blk_id] = grad_logdet;
      }
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        (*grad_out)[blk.theta_idx0[k]] += grad_logdet[k];
      }
    }
  }

  return std::isfinite(lp_out);
}

inline MatrixXd subset_square_1based(const MatrixXd& x, const IntegerVector& idx1) {
  MatrixXd out(idx1.size(), idx1.size());
  for (int j = 0; j < idx1.size(); ++j) {
    for (int i = 0; i < idx1.size(); ++i) {
      out(i, j) = x(idx1[i] - 1, idx1[j] - 1);
    }
  }
  return out;
}

inline VectorXd subset_vec_1based(const VectorXd& x, const IntegerVector& idx1) {
  VectorXd out(idx1.size());
  for (int i = 0; i < idx1.size(); ++i) {
    out(i) = x(idx1[i] - 1);
  }
  return out;
}

struct TwoLevelImplied_ {
  MatrixXd sigma_w;
  MatrixXd sigma_b;
  MatrixXd sigma_zz;
  MatrixXd sigma_yz;
  VectorXd mu_z;
  VectorXd mu_y;
};

inline bool build_twolevel_implied(
    const List& group,
    const NumericVector& x_free,
    TwoLevelImplied_& out) {
  const List within = group["within"];
  const List between = group["between"];
  const List cluster_stats = group["cluster_stats"];
  const List lp = cluster_stats["lp"];

  const MatrixXd SigmaW = lisrel_implied_cov_group_impl(within, x_free);
  const MatrixXd SigmaB = lisrel_implied_cov_group_impl(between, x_free);
  if (SigmaW.size() == 0 || SigmaB.size() == 0) {
    return false;
  }

  MatrixXd MuWm = lisrel_implied_mean_group_impl(within, x_free);
  MatrixXd MuBm = lisrel_implied_mean_group_impl(between, x_free);
  if (MuWm.size() == 0 || MuBm.size() == 0) {
    return false;
  }

  const IntegerVector both_idx = lp["both_idx"];
  const IntegerVector within_idx = lp["within_idx"];
  const IntegerVector between_idx = lp["between_idx"];
  const List ov_idx = lp["ov_idx"];
  const IntegerVector ov_idx_w = ov_idx[0];
  const IntegerVector ov_idx_b = ov_idx[1];

  int p_tilde = 0;
  for (int i = 0; i < ov_idx_w.size(); ++i) p_tilde = std::max(p_tilde, ov_idx_w[i]);
  for (int i = 0; i < ov_idx_b.size(); ++i) p_tilde = std::max(p_tilde, ov_idx_b[i]);

  MatrixXd SigmaW_tilde = MatrixXd::Zero(p_tilde, p_tilde);
  MatrixXd SigmaB_tilde = MatrixXd::Zero(p_tilde, p_tilde);
  VectorXd MuW_tilde = VectorXd::Zero(p_tilde);
  VectorXd MuB_tilde = VectorXd::Zero(p_tilde);

  set_submatrix(SigmaW_tilde, ov_idx_w, SigmaW);
  set_submatrix(SigmaB_tilde, ov_idx_b, SigmaB);
  set_subvector(MuW_tilde, ov_idx_w, MuWm);
  set_subvector(MuB_tilde, ov_idx_b, MuBm);

  VectorXd MuWB_tilde = VectorXd::Zero(p_tilde);
  for (int i = 0; i < within_idx.size(); ++i) {
    MuWB_tilde(within_idx[i] - 1) = MuW_tilde(within_idx[i] - 1);
  }
  for (int i = 0; i < both_idx.size(); ++i) {
    const int idx = both_idx[i] - 1;
    MuWB_tilde(idx) = MuB_tilde(idx) + MuW_tilde(idx);
    MuW_tilde(idx) = 0.0;
    MuB_tilde(idx) = MuWB_tilde(idx);
  }
  for (int i = 0; i < within_idx.size(); ++i) {
    MuB_tilde(within_idx[i] - 1) = 0.0;
  }

  if (between_idx.size() > 0) {
    std::vector<int> not_between;
    not_between.reserve(p_tilde - between_idx.size());
    std::vector<bool> is_between(p_tilde, false);
    for (int i = 0; i < between_idx.size(); ++i) is_between[between_idx[i] - 1] = true;
    for (int i = 0; i < p_tilde; ++i) if (!is_between[i]) not_between.push_back(i);

    out.mu_z = subset_vec_1based(MuB_tilde, between_idx);
    out.mu_y.resize(not_between.size());
    for (int i = 0; i < static_cast<int>(not_between.size()); ++i) out.mu_y(i) = MuWB_tilde(not_between[i]);
    out.sigma_zz = subset_square_1based(SigmaB_tilde, between_idx);

    out.sigma_yz.resize(not_between.size(), between_idx.size());
    out.sigma_b.resize(not_between.size(), not_between.size());
    out.sigma_w.resize(not_between.size(), not_between.size());
    for (int j = 0; j < between_idx.size(); ++j) {
      for (int i = 0; i < static_cast<int>(not_between.size()); ++i) {
        out.sigma_yz(i, j) = SigmaB_tilde(not_between[i], between_idx[j] - 1);
      }
    }
    for (int j = 0; j < static_cast<int>(not_between.size()); ++j) {
      for (int i = 0; i < static_cast<int>(not_between.size()); ++i) {
        out.sigma_b(i, j) = SigmaB_tilde(not_between[i], not_between[j]);
        out.sigma_w(i, j) = SigmaW_tilde(not_between[i], not_between[j]);
      }
    }
  } else {
    out.mu_z.resize(0);
    out.mu_y = MuWB_tilde;
    out.sigma_zz.resize(0, 0);
    out.sigma_yz.resize(p_tilde, 0);
    out.sigma_b = SigmaB_tilde;
    out.sigma_w = SigmaW_tilde;
  }

  return out.sigma_w.rows() > 0;
}

inline double lisrel_loglik_twolevel_group_impl(const List& group, const NumericVector& x_free) {
  TwoLevelImplied_ imp;
  if (!build_twolevel_implied(group, x_free, imp)) {
    return -1e40;
  }

  const List cluster_stats = group["cluster_stats"];
  const List lp = cluster_stats["lp"];
  const IntegerVector between_idx = lp["between_idx"];
  const IntegerVector both_idx = lp["both_idx"];
  const IntegerVector within_idx = lp["within_idx"];
  IntegerVector nobs_v = lp["nobs"];
  IntegerVector nclusters_v = lp["nclusters"];
  LogicalVector has_x_v = lp["has_x"];
  const IntegerVector cluster_size = lp["cluster_size"];
  const IntegerVector cluster_sizes = lp["cluster_sizes"];
  const IntegerVector cluster_size_ns = lp["cluster_size_ns"];
  const NumericMatrix S_PW_full = cluster_stats["Sigma_W"];
  const List cov_d = cluster_stats["cov_d"];
  const List mean_d = cluster_stats["mean_d"];
  NumericVector loglik_x_v = cluster_stats["loglik_x"];

  MatrixXd S_PW = as<MatrixXd>(S_PW_full);
  if (between_idx.size() > 0) {
    std::vector<int> not_between;
    std::vector<bool> is_between(S_PW.rows(), false);
    for (int i = 0; i < between_idx.size(); ++i) is_between[between_idx[i] - 1] = true;
    not_between.reserve(S_PW.rows() - between_idx.size());
    for (int i = 0; i < S_PW.rows(); ++i) if (!is_between[i]) not_between.push_back(i);
    MatrixXd tmp(not_between.size(), not_between.size());
    for (int j = 0; j < static_cast<int>(not_between.size()); ++j) {
      for (int i = 0; i < static_cast<int>(not_between.size()); ++i) {
        tmp(i, j) = S_PW(not_between[i], not_between[j]);
      }
    }
    S_PW = tmp;
  }

  Eigen::LLT<MatrixXd> llt_w(imp.sigma_w);
  if (llt_w.info() != Eigen::Success) return -1e40;
  const MatrixXd sigma_w_inv =
    llt_w.solve(MatrixXd::Identity(imp.sigma_w.rows(), imp.sigma_w.cols()));
  if (!sigma_w_inv.allFinite()) return -1e40;
  const double sigma_w_logdet =
    2.0 * llt_w.matrixL().toDenseMatrix().diagonal().array().log().sum();

  MatrixXd sigma_zz_inv;
  double sigma_zz_logdet = 0.0;
  MatrixXd sigma_yz_zi;
  MatrixXd sigma_zi_zy;
  MatrixXd sigma_b_z = imp.sigma_b;
  if (between_idx.size() > 0) {
    Eigen::LLT<MatrixXd> llt_zz(imp.sigma_zz);
    if (llt_zz.info() != Eigen::Success) return -1e40;
    sigma_zz_inv =
      llt_zz.solve(MatrixXd::Identity(imp.sigma_zz.rows(), imp.sigma_zz.cols()));
    if (!sigma_zz_inv.allFinite()) return -1e40;
    sigma_zz_logdet =
      2.0 * llt_zz.matrixL().toDenseMatrix().diagonal().array().log().sum();
    sigma_yz_zi = imp.sigma_yz * sigma_zz_inv;
    sigma_zi_zy = sigma_yz_zi.transpose();
    sigma_b_z = imp.sigma_b - imp.sigma_yz * sigma_zi_zy;
  }

  VectorXd mu_concat(imp.mu_z.size() + imp.mu_y.size());
  for (int i = 0; i < imp.mu_z.size(); ++i) mu_concat(i) = imp.mu_z(i);
  for (int i = 0; i < imp.mu_y.size(); ++i) mu_concat(imp.mu_z.size() + i) = imp.mu_y(i);

  double sum_L = 0.0;
  double sum_B = 0.0;
  for (int clz = 0; clz < cov_d.size(); ++clz) {
    const int nj = cluster_sizes[clz];
    const MatrixXd cov_d_mat = as<MatrixXd>(cov_d[clz]);
    NumericVector mean_d_vec = mean_d[clz];
    VectorXd mean_d_e(mean_d_vec.size());
    for (int i = 0; i < mean_d_vec.size(); ++i) mean_d_e(i) = mean_d_vec[i];
    const VectorXd diff = mean_d_e - mu_concat;
    MatrixXd Y2Yc = cov_d_mat + diff * diff.transpose();

    MatrixXd Y2Yc_zz;
    MatrixXd Y2Yc_yz;
    MatrixXd Y2Yc_yy;
    if (between_idx.size() > 0) {
      const int nz = between_idx.size();
      Y2Yc_zz = Y2Yc.topLeftCorner(nz, nz);
      Y2Yc_yz = Y2Yc.bottomLeftCorner(Y2Yc.rows() - nz, nz);
      Y2Yc_yy = Y2Yc.bottomRightCorner(Y2Yc.rows() - nz, Y2Yc.cols() - nz);
    } else {
      Y2Yc_yy = Y2Yc;
    }

    const MatrixXd sigma_j = nj * sigma_b_z + imp.sigma_w;
    Eigen::LLT<MatrixXd> llt_j(sigma_j);
    if (llt_j.info() != Eigen::Success) return -1e40;
    const MatrixXd sigma_j_inv =
      llt_j.solve(MatrixXd::Identity(sigma_j.rows(), sigma_j.cols()));
    if (!sigma_j_inv.allFinite()) return -1e40;
    const double sigma_j_logdet =
      2.0 * llt_j.matrixL().toDenseMatrix().diagonal().array().log().sum();

    const double L = sigma_zz_logdet + sigma_j_logdet;
    double q_zz = 0.0;
    double q_yz = 0.0;
    if (between_idx.size() > 0) {
      const MatrixXd sigma_ji_yz_zi = sigma_j_inv * sigma_yz_zi;
      const MatrixXd Vinv11 = sigma_zz_inv + nj * (sigma_zi_zy * sigma_ji_yz_zi);
      q_zz = (Vinv11.cwiseProduct(Y2Yc_zz)).sum();
      q_yz = -nj * (sigma_ji_yz_zi.cwiseProduct(Y2Yc_yz)).sum();
    }
    const double q_yyc = -nj * (sigma_j_inv.cwiseProduct(Y2Yc_yy)).sum();
    const double B = q_zz + 2.0 * q_yz - q_yyc;

    sum_L += L * cluster_size_ns[clz];
    sum_B += B * cluster_size_ns[clz];
  }

  double cluster_size_m1_sum = 0.0;
  for (int i = 0; i < cluster_size.size(); ++i) cluster_size_m1_sum += (cluster_size[i] - 1.0);
  const double q_W = cluster_size_m1_sum * (sigma_w_inv.cwiseProduct(S_PW)).sum();
  const double L_W = cluster_size_m1_sum * sigma_w_logdet;

  const double LOG_2PI = std::log(2.0 * M_PI);
  const double P =
    static_cast<double>(nobs_v[0]) * static_cast<double>(both_idx.size() + within_idx.size()) +
    static_cast<double>(nclusters_v[0]) * static_cast<double>(between_idx.size());
  double out = -0.5 * (P * LOG_2PI + sum_L + sum_B + q_W + L_W);
  if (has_x_v[0]) {
    out -= loglik_x_v[0];
  }
  return R_finite(out) ? out : -1e40;
}

inline NumericVector lisrel_grad_group_impl(const List& group, const NumericVector& x_free, const int npar) {
  List matrices = group["matrices"];
  const bool has_beta = as<bool>(group["has_beta"]);
  const bool has_mean = as<bool>(group["has_mean"]);

  const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
  const MatrixXd theta = reconstruct_free_matrix(matrices["theta"], x_free);
  const MatrixXd psi = reconstruct_free_matrix(matrices["psi"], x_free);
  MatrixXd nu;
  MatrixXd alpha;
  MatrixXd beta;

  MatrixXd Ainv;
  MatrixXd veta = psi;
  MatrixXd eta_mean;
  if (has_beta) {
    beta = reconstruct_free_matrix(matrices["beta"], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) {
      return NumericVector(x_free.size(), NA_REAL);
    }
    Ainv = lu.inverse();
    veta = Ainv * psi * Ainv.transpose();
    if (has_mean) {
      eta_mean = Ainv * reconstruct_free_matrix(matrices["alpha"], x_free);
    } else {
      eta_mean = MatrixXd::Zero(beta.rows(), 1);
    }
  } else if (has_mean) {
    eta_mean = reconstruct_free_matrix(matrices["alpha"], x_free);
  }

  if (has_mean) {
    nu = reconstruct_free_matrix(matrices["nu"], x_free);
    alpha = reconstruct_free_matrix(matrices["alpha"], x_free);
  }

  const MatrixXd sigma = lambda * veta * lambda.transpose() + theta;
  MatrixXd mu = MatrixXd::Zero(lambda.rows(), 1);
  if (has_mean) {
    mu = nu + lambda * eta_mean;
  }
  Eigen::LLT<MatrixXd> llt(sigma);
  if (llt.info() != Eigen::Success) {
    return NumericVector(x_free.size(), NA_REAL);
  }

  const MatrixXd sample_cov = as<MatrixXd>(group["sample_cov"]);
  const double nobs = as<double>(group["nobs"]);
  MatrixXd sample_mean = MatrixXd::Zero(lambda.rows(), 1);
  if (has_mean) {
    sample_mean = as<MatrixXd>(group["sample_mean"]);
  }
  const MatrixXd sigma_inv = llt.solve(MatrixXd::Identity(sigma.rows(), sigma.cols()));
  if (llt.info() != Eigen::Success || !sigma_inv.allFinite()) {
    return NumericVector(x_free.size(), NA_REAL);
  }

  MatrixXd S_centered = sample_cov;
  MatrixXd d_ll_d_mu = MatrixXd::Zero(lambda.rows(), 1);
  if (has_mean) {
    const MatrixXd diff = sample_mean - mu;
    S_centered = S_centered + diff * diff.transpose();
    d_ll_d_mu = nobs * sigma_inv * diff;
  }

  const MatrixXd d_ll_d_sigma = 0.5 * nobs * (sigma_inv * S_centered * sigma_inv - sigma_inv);
  const MatrixXd d_ll_d_theta = d_ll_d_sigma;
  const MatrixXd M = lambda.transpose() * d_ll_d_sigma * lambda;
  MatrixXd d_ll_d_lambda = 2.0 * d_ll_d_sigma * lambda * veta;
  MatrixXd d_ll_d_nu = MatrixXd::Zero(0, 0);
  MatrixXd d_ll_d_alpha = MatrixXd::Zero(0, 0);
  const MatrixXd d_ll_d_psi = has_beta ? Ainv.transpose() * M * Ainv : M;
  MatrixXd d_ll_d_beta;
  MatrixXd d_ll_d_beta_mean;
  if (has_beta) {
    d_ll_d_beta = 2.0 * Ainv.transpose() * M * veta;
  }
  if (has_mean) {
    d_ll_d_lambda += d_ll_d_mu * eta_mean.transpose();
    d_ll_d_nu = d_ll_d_mu;
    if (has_beta) {
      d_ll_d_alpha = Ainv.transpose() * lambda.transpose() * d_ll_d_mu;
    } else {
      d_ll_d_alpha = lambda.transpose() * d_ll_d_mu;
    }
    if (has_beta) {
      d_ll_d_beta_mean = Ainv.transpose() * lambda.transpose() * d_ll_d_mu * eta_mean.transpose();
      d_ll_d_beta += d_ll_d_beta_mean;
    }
  }

  NumericVector grad(npar);
  grad.fill(0.0);

  auto accumulate_grad = [&grad](const List& spec, const MatrixXd& gmat) {
    IntegerVector free_x_idx = spec["free_x_idx"];
    IntegerVector free_pos_idx = spec["free_pos_idx"];
    const int nrow = as<int>(spec["nrow"]);
    for (int k = 0; k < free_x_idx.size(); ++k) {
      const int pos = free_pos_idx[k] - 1;
      const int i = pos % nrow;
      const int j = pos / nrow;
      grad[free_x_idx[k] - 1] += gmat(i, j);
    }
  };

  accumulate_grad(matrices["lambda"], d_ll_d_lambda);
  accumulate_grad(matrices["theta"], d_ll_d_theta);
  accumulate_grad(matrices["psi"], d_ll_d_psi);
  if (has_beta) {
    accumulate_grad(matrices["beta"], d_ll_d_beta);
  }
  if (has_mean) {
    accumulate_grad(matrices["nu"], d_ll_d_nu);
    accumulate_grad(matrices["alpha"], d_ll_d_alpha);
  }

  return grad;
}

inline void skewnorm_weights(
    const NumericVector& y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp,
    const double logk,
    VectorXd& w) {
  const double kval = is_est_k ? std::exp(logk) : temp;
  const R_xlen_t n = y.size();
  double wsum = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    double wi = 0.0;
    if (y[i] >= threshold_log_drop) {
      wi = std::exp(kval * y[i]);
    }
    w[i] = wi;
    wsum += wi;
  }
  if (wsum <= 0.0 || !R_finite(wsum)) {
    const double uniform_w = 1.0 / static_cast<double>(n);
    w.setConstant(uniform_w);
    return;
  }
  w /= wsum;
}

inline double fit_skew_normal_objective_impl(
    const NumericVector& param,
    const NumericVector& x,
    const NumericVector& y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp) {
  const double mu = param[0];
  const double lsinv = param[1];
  const double a = param[2];
  const double logC = param[3];
  const double logk = is_est_k ? param[4] : std::log(temp);

  VectorXd w(y.size());
  skewnorm_weights(y, threshold_log_drop, is_est_k, temp, logk, w);

  const double s = std::exp(lsinv);
  double out = 0.0;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    const double xx = (x[i] - mu) * s;
    const double logdens = std::log(2.0) +
      R::dnorm4(xx, 0.0, 1.0, 1) +
      R::pnorm5(a * xx, 0.0, 1.0, 1, 1) +
      lsinv +
      logC;
    const double r = y[i] - logdens;
    out += w[i] * r * r;
  }
  return out;
}

inline NumericVector fit_skew_normal_gradient_impl(
    const NumericVector& param,
    const NumericVector& x,
    const NumericVector& y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp) {
  const double mu = param[0];
  const double lsinv = param[1];
  const double a = param[2];
  const double logC = param[3];
  const double s = std::exp(lsinv);
  const double logk = is_est_k ? param[4] : std::log(temp);

  VectorXd w(y.size());
  skewnorm_weights(y, threshold_log_drop, is_est_k, temp, logk, w);

  double g1 = 0.0;
  double g2 = 0.0;
  double g3 = 0.0;
  double g4 = 0.0;
  double g5 = 0.0;

  for (R_xlen_t i = 0; i < x.size(); ++i) {
    const double xx = s * (x[i] - mu);
    const double u = a * xx;
    const double Rv = mills_ratio_scalar(u);

    const double L_mu = s * xx - a * s * Rv;
    const double L_lsinv = -xx * xx + a * xx * Rv + 1.0;
    const double L_a = xx * Rv;
    const double L = std::log(2.0) +
      R::dnorm4(xx, 0.0, 1.0, 1) +
      R::pnorm5(u, 0.0, 1.0, 1, 1) +
      lsinv +
      logC;
    const double r = y[i] - L;
    const double wi = w[i];

    g1 += wi * r * L_mu;
    g2 += wi * r * L_lsinv;
    g3 += wi * r * L_a;
    g4 += wi * r;
    g5 += y[i] * wi * r * r;
  }

  if (is_est_k) {
    return NumericVector::create(
      -2.0 * g1,
      -2.0 * g2,
      -2.0 * g3,
      -2.0 * g4,
      g5 * std::exp(logk)
    );
  }

  return NumericVector::create(
    -2.0 * g1,
    -2.0 * g2,
    -2.0 * g3,
    -2.0 * g4
  );
}

inline NumericMatrix fit_skew_normal_hessian_impl(
    const NumericVector& param,
    const NumericVector& x,
    const NumericVector& y,
    const double threshold_log_drop,
    const double temp) {
  VectorXd w(y.size());
  skewnorm_weights(y, threshold_log_drop, false, temp, 0.0, w);

  const double mu = param[0];
  const double lsinv = param[1];
  const double a = param[2];
  const double logC = param[3];
  const double s = std::exp(lsinv);

  double H11 = 0.0, H12 = 0.0, H13 = 0.0, H14 = 0.0;
  double H22 = 0.0, H23 = 0.0, H24 = 0.0;
  double H33 = 0.0, H34 = 0.0, H44 = 0.0;

  for (R_xlen_t i = 0; i < x.size(); ++i) {
    const double xx = s * (x[i] - mu);
    const double u = a * xx;
    const double Rv = mills_ratio_scalar(u);
    const double Rprime = -Rv * (u + Rv);

    const double L_mu = s * xx - a * s * Rv;
    const double L_lsinv = -xx * xx + a * xx * Rv + 1.0;
    const double L_a = xx * Rv;
    const double L_logC = 1.0;

    const double L_mumu = -s * s + a * a * s * s * Rprime;
    const double L_mu_lsinv = 2.0 * s * xx - a * s * Rv - a * a * s * xx * Rprime;
    const double L_mu_a = -s * Rv - a * s * xx * Rprime;
    const double L_lsinv_lsinv = -2.0 * xx * xx + a * xx * Rv + a * a * xx * xx * Rprime;
    const double L_lsinv_a = xx * Rv + a * xx * xx * Rprime;
    const double L_aa = xx * xx * Rprime;

    const double L = std::log(2.0) +
      R::dnorm4(xx, 0.0, 1.0, 1) +
      R::pnorm5(u, 0.0, 1.0, 1, 1) +
      lsinv +
      logC;
    const double r = y[i] - L;
    const double wi = w[i];

    H11 += wi * (L_mu * L_mu - r * L_mumu);
    H12 += wi * (L_mu * L_lsinv - r * L_mu_lsinv);
    H13 += wi * (L_mu * L_a - r * L_mu_a);
    H14 += wi * (L_mu * L_logC);

    H22 += wi * (L_lsinv * L_lsinv - r * L_lsinv_lsinv);
    H23 += wi * (L_lsinv * L_a - r * L_lsinv_a);
    H24 += wi * (L_lsinv * L_logC);

    H33 += wi * (L_a * L_a - r * L_aa);
    H34 += wi * (L_a * L_logC);

    H44 += wi * (L_logC * L_logC);
  }

  NumericMatrix out(4, 4);
  out(0, 0) = 2.0 * H11; out(0, 1) = 2.0 * H12; out(0, 2) = 2.0 * H13; out(0, 3) = 2.0 * H14;
  out(1, 0) = 2.0 * H12; out(1, 1) = 2.0 * H22; out(1, 2) = 2.0 * H23; out(1, 3) = 2.0 * H24;
  out(2, 0) = 2.0 * H13; out(2, 1) = 2.0 * H23; out(2, 2) = 2.0 * H33; out(2, 3) = 2.0 * H34;
  out(3, 0) = 2.0 * H14; out(3, 1) = 2.0 * H24; out(3, 2) = 2.0 * H34; out(3, 3) = 2.0 * H44;
  return out;
}

template <typename FG>
inline List optimize_lbfgs_impl(
    const NumericVector& par0,
    FG&& fg,
    const int max_iter = 200,
    const double grad_tol = 1e-8,
    const int m_bfgs = 10,
    const int max_ls = 40,
    const double h = 1e-5) {
  const int n = par0.size();
  VectorXd pars(n);
  for (int i = 0; i < n; ++i) pars[i] = par0[i];

  double f = 0.0;
  VectorXd g(n);
  if (!fg(pars, f, g)) {
    return List::create(
      Named("par") = par0,
      Named("objective") = R_PosInf,
      Named("gradient") = NumericVector(n, NA_REAL),
      Named("hessian") = NumericMatrix(n, n),
      Named("converged") = false,
      Named("n_iter") = 0
    );
  }

  std::vector<VectorXd> sv, yv;
  std::vector<double> rhov;
  sv.reserve(m_bfgs);
  yv.reserve(m_bfgs);
  rhov.reserve(m_bfgs);

  int n_iter = 0;
  bool converged = false;

  while (n_iter < max_iter) {
    if (g.cwiseAbs().maxCoeff() < grad_tol) {
      converged = true;
      break;
    }

    int k = static_cast<int>(sv.size());
    VectorXd q = g;
    std::vector<double> alps(k);
    for (int i = k - 1; i >= 0; --i) {
      alps[i] = rhov[i] * sv[i].dot(q);
      q -= alps[i] * yv[i];
    }
    VectorXd r = q;
    if (k > 0) {
      double gamma = sv[k - 1].dot(yv[k - 1]) / yv[k - 1].squaredNorm();
      if (gamma > 0.0 && std::isfinite(gamma)) r *= gamma;
    }
    for (int i = 0; i < k; ++i) {
      double beta = rhov[i] * yv[i].dot(r);
      r += sv[i] * (alps[i] - beta);
    }
    VectorXd dir = -r;

    double slope = g.dot(dir);
    if (!std::isfinite(slope) || slope >= 0.0) {
      sv.clear();
      yv.clear();
      rhov.clear();
      dir = -g;
      slope = g.dot(dir);
    }

    double alpha_ls = 1.0;
    double f_new = 0.0;
    VectorXd g_new(n);
    bool ls_ok = false;
    for (int i = 0; i < max_ls; ++i) {
      VectorXd pars_new = pars + alpha_ls * dir;
      if (fg(pars_new, f_new, g_new) && f_new <= f + 1e-4 * alpha_ls * slope) {
        ls_ok = true;
        break;
      }
      alpha_ls *= 0.5;
      if (alpha_ls < 1e-16) break;
    }
    if (!ls_ok) break;

    VectorXd s = alpha_ls * dir;
    VectorXd y = g_new - g;
    double sy = s.dot(y);
    if (sy > 1e-10 * s.norm() * y.norm()) {
      if (k == m_bfgs) {
        sv.erase(sv.begin());
        yv.erase(yv.begin());
        rhov.erase(rhov.begin());
      }
      sv.push_back(s);
      yv.push_back(y);
      rhov.push_back(1.0 / sy);
    }

    pars += s;
    f = f_new;
    g = g_new;
    ++n_iter;
  }

  MatrixXd H(n, n);
  H.setZero();
  for (int j = 0; j < n; ++j) {
    VectorXd pp = pars;
    pp[j] += h;
    double fp = 0.0;
    VectorXd gp(n);
    bool okp = fg(pp, fp, gp);
    for (int i = 0; i < n; ++i) {
      H(i, j) = okp ? (gp[i] - g[i]) / h : 0.0;
    }
  }
  H = 0.5 * (H + H.transpose());

  NumericVector par_out(n);
  NumericVector grad_out(n);
  for (int i = 0; i < n; ++i) {
    par_out[i] = pars[i];
    grad_out[i] = g[i];
  }

  return List::create(
    Named("par") = par_out,
    Named("objective") = f,
    Named("gradient") = grad_out,
    Named("hessian") = wrap(H),
    Named("converged") = converged,
    Named("n_iter") = n_iter
  );
}

} // namespace

// [[Rcpp::export]]
NumericVector cpp_mills_ratio(NumericVector u) {
  NumericVector out(u.size());
  for (R_xlen_t i = 0; i < u.size(); ++i) {
    out[i] = mills_ratio_scalar(u[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector cpp_dsnorm(
    NumericVector x,
    const double xi,
    const double omega,
    const double alpha,
    const double logC = 0.0,
    const bool log_out = false) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    const double logdens = skewnorm_logdens_scalar(x[i], xi, omega, alpha, logC);
    out[i] = log_out ? logdens : std::exp(logdens);
  }
  return out;
}

// [[Rcpp::export]]
double cpp_fit_skew_normal_objective(
    NumericVector param,
    NumericVector x,
    NumericVector y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp) {
  return fit_skew_normal_objective_impl(param, x, y, threshold_log_drop, is_est_k, temp);
}

// [[Rcpp::export]]
NumericVector cpp_fit_skew_normal_gradient(
    NumericVector param,
    NumericVector x,
    NumericVector y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp) {
  return fit_skew_normal_gradient_impl(param, x, y, threshold_log_drop, is_est_k, temp);
}

// [[Rcpp::export]]
NumericMatrix cpp_fit_skew_normal_hessian(
    NumericVector param,
    NumericVector x,
    NumericVector y,
    const double threshold_log_drop,
    const double temp) {
  return fit_skew_normal_hessian_impl(param, x, y, threshold_log_drop, temp);
}

// [[Rcpp::export]]
List cpp_fit_skew_normal_optimize(
    NumericVector par0,
    NumericVector x,
    NumericVector y,
    const double threshold_log_drop,
    const bool is_est_k,
    const double temp,
    const int max_iter = 200,
    const double grad_tol = 1e-8,
    const int m_bfgs = 10,
    const int max_ls = 40,
    const double h = 1e-5) {
  auto fg = [&](const VectorXd& pars, double& fval, VectorXd& gval) -> bool {
    NumericVector par(pars.size());
    for (int i = 0; i < pars.size(); ++i) par[i] = pars[i];

    fval = fit_skew_normal_objective_impl(par, x, y, threshold_log_drop, is_est_k, temp);
    if (!R_finite(fval)) return false;

    NumericVector gr = fit_skew_normal_gradient_impl(par, x, y, threshold_log_drop, is_est_k, temp);
    gval.resize(gr.size());
    for (int i = 0; i < gr.size(); ++i) gval[i] = gr[i];
    return gval.allFinite();
  };

  List opt = optimize_lbfgs_impl(par0, fg, max_iter, grad_tol, m_bfgs, max_ls, h);
  if (!is_est_k) {
    opt["hessian"] = fit_skew_normal_hessian_impl(
      as<NumericVector>(opt["par"]),
      x,
      y,
      threshold_log_drop,
      temp
    );
  }
  return opt;
}

// [[Rcpp::export]]
double cpp_fit_skew_normal_sample_nll(NumericVector par, NumericVector x) {
  const double xi = par[0];
  const double omega = std::exp(par[1]);
  const double alpha = par[2];
  double out = 0.0;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out -= skewnorm_logdens_scalar(x[i], xi, omega, alpha, 0.0);
  }
  return out;
}

// [[Rcpp::export]]
List cpp_fit_skew_normal_sample_optimize(
    NumericVector par0,
    NumericVector x,
    const int max_iter = 200,
    const double grad_tol = 1e-8,
    const int m_bfgs = 10,
    const int max_ls = 40,
    const double h = 1e-5) {
  auto fg = [&](const VectorXd& pars, double& fval, VectorXd& gval) -> bool {
    NumericVector par(pars.size());
    for (int i = 0; i < pars.size(); ++i) par[i] = pars[i];

    fval = cpp_fit_skew_normal_sample_nll(par, x);
    if (!R_finite(fval)) return false;

    gval.resize(pars.size());
    for (int j = 0; j < pars.size(); ++j) {
      NumericVector xp = clone(par);
      NumericVector xm = clone(par);
      xp[j] += h;
      xm[j] -= h;
      const double fp = cpp_fit_skew_normal_sample_nll(xp, x);
      const double fm = cpp_fit_skew_normal_sample_nll(xm, x);
      if (!R_finite(fp) || !R_finite(fm)) return false;
      gval[j] = (fp - fm) / (2.0 * h);
    }
    return gval.allFinite();
  };

  return optimize_lbfgs_impl(par0, fg, max_iter, grad_tol, m_bfgs, max_ls, h);
}

// [[Rcpp::export]]
NumericVector cpp_fast_grad(Function fn, NumericVector x, const double h = 1e-5) {
  const int m = x.size();
  NumericVector g(m);
  for (int j = 0; j < m; ++j) {
    NumericVector xp = clone(x);
    NumericVector xm = clone(x);
    xp[j] += h;
    xm[j] -= h;
    g[j] = (as<double>(fn(xp)) - as<double>(fn(xm))) / (2.0 * h);
  }
  return g;
}

// [[Rcpp::export]]
NumericMatrix cpp_fast_jacobian(Function gr_fn, NumericVector x, const double h = 1e-5) {
  const int m = x.size();
  MatrixXd H = MatrixXd::Zero(m, m);
  for (int j = 0; j < m; ++j) {
    NumericVector xp = clone(x);
    NumericVector xm = clone(x);
    xp[j] += h;
    xm[j] -= h;
    NumericVector gp = gr_fn(xp);
    NumericVector gm = gr_fn(xm);
    for (int i = 0; i < m; ++i) {
      H(i, j) = (gp[i] - gm[i]) / (2.0 * h);
    }
  }
  return wrap(H);
}

// [[Rcpp::export]]
NumericMatrix cpp_fast_hessian(Function fn, NumericVector x, const double h = 1e-5) {
  const int m = x.size();
  MatrixXd H = MatrixXd::Zero(m, m);
  const double f0 = as<double>(fn(x));

  for (int i = 0; i < m; ++i) {
    NumericVector xp = clone(x);
    NumericVector xm = clone(x);
    xp[i] += h;
    xm[i] -= h;
    H(i, i) = (as<double>(fn(xp)) - 2.0 * f0 + as<double>(fn(xm))) / (h * h);
  }

  for (int i = 0; i < m - 1; ++i) {
    for (int j = i + 1; j < m; ++j) {
      NumericVector xpp = clone(x);
      NumericVector xpm = clone(x);
      NumericVector xmp = clone(x);
      NumericVector xmm = clone(x);
      xpp[i] += h; xpp[j] += h;
      xpm[i] += h; xpm[j] -= h;
      xmp[i] -= h; xmp[j] += h;
      xmm[i] -= h; xmm[j] -= h;
      const double hij = (
        as<double>(fn(xpp)) -
        as<double>(fn(xpm)) -
        as<double>(fn(xmp)) +
        as<double>(fn(xmm))
      ) / (4.0 * h * h);
      H(i, j) = hij;
      H(j, i) = hij;
    }
  }

  return wrap(H);
}

// [[Rcpp::export]]
NumericMatrix cpp_lisrel_implied_cov(List model, NumericVector x_free) {
  List groups = model["groups"];
  MatrixXd sigma = lisrel_implied_cov_group_impl(groups[0], x_free);
  if (sigma.size() == 0) {
    stop("Native LISREL backend failed to invert I-Beta.");
  }
  return wrap(sigma);
}

// [[Rcpp::export]]
NumericMatrix cpp_lisrel_implied_mean(List model, NumericVector x_free) {
  List groups = model["groups"];
  MatrixXd mu = lisrel_implied_mean_group_impl(groups[0], x_free);
  if (mu.size() == 0) {
    stop("Native LISREL backend failed to invert I-Beta for means.");
  }
  return wrap(mu);
}

// [[Rcpp::export]]
List cpp_lisrel_latent_posterior(List model, NumericVector x_free) {
  List groups = model["groups"];
  const int ng = groups.size();
  List out(ng);

  for (int g = 0; g < ng; ++g) {
    List group = groups[g];
    if (!group.containsElementNamed("Y")) {
      stop("Native latent posterior requires group data `Y`.");
    }

    const NumericMatrix Y = as<NumericMatrix>(group["Y"]);
    List matrices = group["matrices"];
    const bool has_beta = as<bool>(group["has_beta"]);
    const bool has_mean = as<bool>(group["has_mean"]);

    const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
    const MatrixXd theta = reconstruct_free_matrix(matrices["theta"], x_free);
    const MatrixXd psi = reconstruct_free_matrix(matrices["psi"], x_free);

    MatrixXd Phi = psi;
    MatrixXd front = lambda;
    if (has_beta) {
      const MatrixXd beta = reconstruct_free_matrix(matrices["beta"], x_free);
      const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
      Eigen::FullPivLU<MatrixXd> lu(A);
      if (!lu.isInvertible()) {
        stop("Native latent posterior failed to invert I-Beta.");
      }
      const MatrixXd Ainv = lu.inverse();
      Phi = Ainv * psi * Ainv.transpose();
      front = lambda * Ainv;
    }

    VectorXd alpha = VectorXd::Zero(lambda.cols());
    if (has_mean) {
      alpha = reconstruct_free_matrix(matrices["alpha"], x_free).col(0);
    }

    const int n_obs = Y.nrow();
    const int p = Y.ncol();
    const int nlv = lambda.cols();
    NumericMatrix eta_mean(n_obs, nlv);
    List eta_cov(n_obs);

    for (int i = 0; i < n_obs; ++i) {
      std::vector<int> obs_idx;
      obs_idx.reserve(p);
      for (int j = 0; j < p; ++j) {
        if (R_finite(Y(i, j))) obs_idx.push_back(j);
      }

      if ((int)obs_idx.size() == 0) {
        for (int k = 0; k < nlv; ++k) eta_mean(i, k) = alpha[k];
        eta_cov[i] = wrap(Phi);
        continue;
      }

      MatrixXd lambda_obs(obs_idx.size(), lambda.cols());
      MatrixXd front_obs(obs_idx.size(), front.cols());
      MatrixXd theta_obs(obs_idx.size(), obs_idx.size());
      VectorXd y_obs(obs_idx.size());

      for (int r = 0; r < (int)obs_idx.size(); ++r) {
        lambda_obs.row(r) = lambda.row(obs_idx[r]);
        front_obs.row(r) = front.row(obs_idx[r]);
        y_obs[r] = Y(i, obs_idx[r]);
        for (int c = 0; c < (int)obs_idx.size(); ++c) {
          theta_obs(r, c) = theta(obs_idx[r], obs_idx[c]);
        }
      }

      const MatrixXd Sigma_y = front_obs * psi * front_obs.transpose() + theta_obs;
      Eigen::FullPivLU<MatrixXd> lu_sigma(Sigma_y);
      if (!lu_sigma.isInvertible()) {
        stop("Native latent posterior failed: implied covariance is singular.");
      }
      const MatrixXd Sigma_y_inv = lu_sigma.inverse();

      const MatrixXd PhiLtSinv = Phi * lambda_obs.transpose() * Sigma_y_inv;
      const VectorXd mu_eta = alpha + PhiLtSinv * y_obs;
      MatrixXd V_eta = Phi - PhiLtSinv * lambda_obs * Phi;
      V_eta = 0.5 * (V_eta + V_eta.transpose());

      for (int k = 0; k < nlv; ++k) eta_mean(i, k) = mu_eta[k];
      eta_cov[i] = wrap(V_eta);
    }

    out[g] = List::create(
      Named("eta_mean") = eta_mean,
      Named("eta_cov") = eta_cov
    );
  }

  return out;
}

// [[Rcpp::export]]
List cpp_lisrel_predict_y(List model, NumericVector x_free, const bool add_noise = false) {
  List groups = model["groups"];
  const int ng = groups.size();
  List out(ng);

  for (int g = 0; g < ng; ++g) {
    List group = groups[g];
    if (!group.containsElementNamed("Y")) {
      stop("Native prediction requires group data `Y`.");
    }

    const NumericMatrix Y = as<NumericMatrix>(group["Y"]);
    List matrices = group["matrices"];
    const bool has_beta = as<bool>(group["has_beta"]);
    const bool has_mean = as<bool>(group["has_mean"]);

    const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
    const MatrixXd theta = reconstruct_free_matrix(matrices["theta"], x_free);
    const MatrixXd psi = reconstruct_free_matrix(matrices["psi"], x_free);

    MatrixXd Phi = psi;
    MatrixXd front = lambda;
    MatrixXd Ainv = MatrixXd::Identity(lambda.cols(), lambda.cols());
    if (has_beta) {
      const MatrixXd beta = reconstruct_free_matrix(matrices["beta"], x_free);
      const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
      Eigen::FullPivLU<MatrixXd> lu(A);
      if (!lu.isInvertible()) {
        stop("Native prediction failed to invert I-Beta.");
      }
      Ainv = lu.inverse();
      Phi = Ainv * psi * Ainv.transpose();
      front = lambda * Ainv;
    }

    VectorXd alpha = VectorXd::Zero(lambda.cols());
    VectorXd nu = VectorXd::Zero(lambda.rows());
    if (has_mean) {
      alpha = reconstruct_free_matrix(matrices["alpha"], x_free).col(0);
      nu = reconstruct_free_matrix(matrices["nu"], x_free).col(0);
    }

    const int n_obs = Y.nrow();
    const int p = Y.ncol();
    const int nlv = lambda.cols();
    NumericMatrix yhat(n_obs, p);

    for (int i = 0; i < n_obs; ++i) {
      std::vector<int> obs_idx;
      obs_idx.reserve(p);
      for (int j = 0; j < p; ++j) {
        if (R_finite(Y(i, j))) obs_idx.push_back(j);
      }

      VectorXd eta_draw = alpha;
      if ((int)obs_idx.size() > 0) {
        MatrixXd lambda_obs(obs_idx.size(), lambda.cols());
        MatrixXd front_obs(obs_idx.size(), front.cols());
        MatrixXd theta_obs(obs_idx.size(), obs_idx.size());
        VectorXd y_obs(obs_idx.size());

        for (int r = 0; r < (int)obs_idx.size(); ++r) {
          lambda_obs.row(r) = lambda.row(obs_idx[r]);
          front_obs.row(r) = front.row(obs_idx[r]);
          y_obs[r] = Y(i, obs_idx[r]);
          for (int c = 0; c < (int)obs_idx.size(); ++c) {
            theta_obs(r, c) = theta(obs_idx[r], obs_idx[c]);
          }
        }

        const MatrixXd Sigma_y = front_obs * psi * front_obs.transpose() + theta_obs;
        Eigen::FullPivLU<MatrixXd> lu_sigma(Sigma_y);
        if (!lu_sigma.isInvertible()) {
          stop("Native prediction failed: implied covariance is singular.");
        }
        const MatrixXd Sigma_y_inv = lu_sigma.inverse();

        const MatrixXd PhiLtSinv = Phi * lambda_obs.transpose() * Sigma_y_inv;
        const VectorXd mu_eta = alpha + PhiLtSinv * y_obs;
        MatrixXd V_eta = Phi - PhiLtSinv * lambda_obs * Phi;
        V_eta = 0.5 * (V_eta + V_eta.transpose());

        Eigen::LLT<MatrixXd> llt_eta(V_eta);
        MatrixXd chol_eta;
        if (llt_eta.info() == Eigen::Success) {
          chol_eta = llt_eta.matrixL();
        } else {
          chol_eta = MatrixXd::Zero(nlv, nlv);
          for (int k = 0; k < nlv; ++k) {
            chol_eta(k, k) = std::sqrt(std::max(V_eta(k, k), 0.0));
          }
        }
        VectorXd z_eta = VectorXd::Zero(nlv);
        for (int k = 0; k < nlv; ++k) z_eta[k] = R::rnorm(0.0, 1.0);
        eta_draw = mu_eta + chol_eta * z_eta;
      }

      VectorXd yhat_i = nu;
      if (has_beta) {
        yhat_i += lambda * (Ainv * eta_draw);
      } else {
        yhat_i += lambda * eta_draw;
      }

      if (add_noise) {
        Eigen::LLT<MatrixXd> llt_theta(theta);
        MatrixXd chol_theta;
        if (llt_theta.info() == Eigen::Success) {
          chol_theta = llt_theta.matrixL();
        } else {
          chol_theta = MatrixXd::Zero(p, p);
          for (int k = 0; k < p; ++k) {
            chol_theta(k, k) = std::sqrt(std::max(theta(k, k), 0.0));
          }
        }
        VectorXd eps = VectorXd::Zero(p);
        for (int k = 0; k < p; ++k) eps[k] = R::rnorm(0.0, 1.0);
        yhat_i += chol_theta * eps;
      }

      for (int j = 0; j < p; ++j) yhat(i, j) = yhat_i[j];
    }

    out[g] = yhat;
  }

  return out;
}

// [[Rcpp::export]]
List cpp_lisrel_impute_missing(List model, NumericVector x_free) {
  List groups = model["groups"];
  const int ng = groups.size();
  List out(ng);

  for (int g = 0; g < ng; ++g) {
    List group = groups[g];
    if (!group.containsElementNamed("Y")) {
      stop("Native imputation requires group data `Y`.");
    }

    const NumericMatrix Y = as<NumericMatrix>(group["Y"]);
    const MatrixXd Sigma_y = lisrel_implied_cov_group_impl(group, x_free);
    if (Sigma_y.size() == 0) {
      stop("Native imputation failed to compute implied covariance.");
    }

    MatrixXd mu_mat = MatrixXd::Zero(Sigma_y.rows(), 1);
    if (as<bool>(group["has_mean"])) {
      mu_mat = lisrel_implied_mean_group_impl(group, x_free);
      if (mu_mat.size() == 0) {
        stop("Native imputation failed to compute implied mean.");
      }
    }
    const VectorXd mu_y = mu_mat.col(0);

    const int n_obs = Y.nrow();
    const int p = Y.ncol();
    NumericMatrix outg(n_obs, p);
    for (int i = 0; i < n_obs; ++i) {
      for (int j = 0; j < p; ++j) outg(i, j) = Y(i, j);
    }

    for (int i = 0; i < n_obs; ++i) {
      std::vector<int> mis_idx;
      std::vector<int> obs_idx;
      mis_idx.reserve(p);
      obs_idx.reserve(p);
      for (int j = 0; j < p; ++j) {
        if (R_finite(Y(i, j))) {
          obs_idx.push_back(j);
        } else {
          mis_idx.push_back(j);
        }
      }

      if ((int)mis_idx.size() == 0) continue;

      MatrixXd Sigma_oo(obs_idx.size(), obs_idx.size());
      MatrixXd Sigma_mo(mis_idx.size(), obs_idx.size());
      MatrixXd Sigma_mm(mis_idx.size(), mis_idx.size());
      VectorXd mu_mis(mis_idx.size());
      VectorXd mu_cond(mis_idx.size());

      for (int r = 0; r < (int)mis_idx.size(); ++r) {
        mu_mis[r] = mu_y[mis_idx[r]];
        for (int c = 0; c < (int)mis_idx.size(); ++c) {
          Sigma_mm(r, c) = Sigma_y(mis_idx[r], mis_idx[c]);
        }
      }

      MatrixXd Sigma_cond = Sigma_mm;
      if ((int)obs_idx.size() > 0) {
        VectorXd y_obs(obs_idx.size());
        VectorXd mu_obs(obs_idx.size());
        for (int r = 0; r < (int)obs_idx.size(); ++r) {
          y_obs[r] = Y(i, obs_idx[r]);
          mu_obs[r] = mu_y[obs_idx[r]];
          for (int c = 0; c < (int)obs_idx.size(); ++c) {
            Sigma_oo(r, c) = Sigma_y(obs_idx[r], obs_idx[c]);
          }
        }
        for (int r = 0; r < (int)mis_idx.size(); ++r) {
          for (int c = 0; c < (int)obs_idx.size(); ++c) {
            Sigma_mo(r, c) = Sigma_y(mis_idx[r], obs_idx[c]);
          }
        }

        Eigen::FullPivLU<MatrixXd> lu_oo(Sigma_oo);
        if (!lu_oo.isInvertible()) {
          stop("Native imputation failed: observed covariance block is singular.");
        }
        const MatrixXd A = Sigma_mo * lu_oo.inverse();
        Sigma_cond = Sigma_mm - A * Sigma_mo.transpose();
        mu_cond = mu_mis + A * (y_obs - mu_obs);
      } else {
        mu_cond = mu_mis;
      }
      Sigma_cond = 0.5 * (Sigma_cond + Sigma_cond.transpose());

      Eigen::LLT<MatrixXd> llt_cond(Sigma_cond);
      if (llt_cond.info() != Eigen::Success) {
        stop("Native imputation failed: conditional covariance is not positive definite.");
      }
      const MatrixXd chol_cond = llt_cond.matrixL();

      VectorXd z = VectorXd::Zero(mis_idx.size());
      for (int k = 0; k < (int)mis_idx.size(); ++k) z[k] = R::rnorm(0.0, 1.0);
      VectorXd draw = mu_cond + chol_cond * z;

      for (int k = 0; k < (int)mis_idx.size(); ++k) {
        outg(i, mis_idx[k]) = draw[k];
      }
    }

    out[g] = outg;
  }

  return out;
}

// Combined loglik + gradient in one pass (avoids factorising Sigma twice).
// Returns false on numerical failure; writes results into ll_out and grad_out.
inline bool lisrel_loglik_and_grad_group_impl(
    const List& group,
    const NumericVector& x_free,
    const int npar,
    double& ll_out,
    NumericVector& grad_out) {

  List matrices = group["matrices"];
  const bool has_beta = as<bool>(group["has_beta"]);
  const bool has_mean = as<bool>(group["has_mean"]);

  const MatrixXd lambda = reconstruct_free_matrix(matrices["lambda"], x_free);
  const MatrixXd theta  = reconstruct_free_matrix(matrices["theta"],  x_free);
  const MatrixXd psi    = reconstruct_free_matrix(matrices["psi"],    x_free);
  MatrixXd nu, alpha, beta;
  MatrixXd Ainv;
  MatrixXd veta = psi;
  MatrixXd eta_mean;

  if (has_beta) {
    beta = reconstruct_free_matrix(matrices["beta"], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return false;
    Ainv     = lu.inverse();
    veta     = Ainv * psi * Ainv.transpose();
    if (has_mean) {
      eta_mean = Ainv * reconstruct_free_matrix(matrices["alpha"], x_free);
    } else {
      eta_mean = MatrixXd::Zero(beta.rows(), 1);
    }
  } else if (has_mean) {
    eta_mean = reconstruct_free_matrix(matrices["alpha"], x_free);
  }

  if (has_mean) {
    nu    = reconstruct_free_matrix(matrices["nu"],    x_free);
    alpha = reconstruct_free_matrix(matrices["alpha"], x_free);
  }

  const MatrixXd sigma = lambda * veta * lambda.transpose() + theta;
  MatrixXd mu = MatrixXd::Zero(lambda.rows(), 1);
  if (has_mean) mu = nu + lambda * eta_mean;
  auto subset_square_local = [](const MatrixXd& x, const std::vector<int>& idx) {
    MatrixXd out(idx.size(), idx.size());
    for (int j = 0; j < static_cast<int>(idx.size()); ++j)
      for (int i = 0; i < static_cast<int>(idx.size()); ++i)
        out(i, j) = x(idx[i], idx[j]);
    return out;
  };
  auto subset_rows_local = [](const MatrixXd& x, const std::vector<int>& idx) {
    MatrixXd out(idx.size(), x.cols());
    for (int i = 0; i < static_cast<int>(idx.size()); ++i)
      out.row(i) = x.row(idx[i]);
    return out;
  };

  MatrixXd d_ll_d_lambda = MatrixXd::Zero(lambda.rows(), lambda.cols());
  MatrixXd d_ll_d_theta  = MatrixXd::Zero(theta.rows(), theta.cols());
  MatrixXd d_ll_d_psi    = MatrixXd::Zero(psi.rows(), psi.cols());
  MatrixXd d_ll_d_nu     = has_mean ? MatrixXd::Zero(lambda.rows(), 1) : MatrixXd();
  MatrixXd d_ll_d_alpha  = has_mean ? MatrixXd::Zero(psi.rows(), 1) : MatrixXd();
  MatrixXd d_ll_d_beta   = has_beta ? MatrixXd::Zero(psi.rows(), psi.cols()) : MatrixXd();
  ll_out = 0.0;

  LogicalVector has_missing_v = group["has_missing"];
  const bool has_missing = has_missing_v[0];
  if (has_missing) {
    const List missing = group["missing"];
    for (int m = 0; m < missing.size(); ++m) {
      const List rec = missing[m];
      const MatrixXd sample_cov = missing_pattern_cov_from_record(rec);
      NumericVector sample_mean_vec = has_mean ? as<NumericVector>(rec["MY"]) : NumericVector(0);
      LogicalVector var_idx = rec["var_idx"];
      IntegerVector freq_v = rec["freq"];
      const double nobs = freq_v[0];

      std::vector<int> obs_idx;
      obs_idx.reserve(var_idx.size());
      for (int i = 0; i < var_idx.size(); ++i) {
        if (var_idx[i]) obs_idx.push_back(i);
      }

      const MatrixXd lambda_obs = subset_rows_local(lambda, obs_idx);
      const MatrixXd theta_obs  = subset_square_local(theta, obs_idx);
      const MatrixXd sigma_obs  = subset_square_local(sigma, obs_idx);
      Eigen::LLT<MatrixXd> llt(sigma_obs);
      if (llt.info() != Eigen::Success) return false;

      const MatrixXd sigma_inv = llt.solve(MatrixXd::Identity(sigma_obs.rows(), sigma_obs.cols()));
      if (!sigma_inv.allFinite()) return false;

      MatrixXd S_centered = sample_cov;
      MatrixXd d_ll_d_mu_obs = MatrixXd::Zero(sigma_obs.rows(), 1);
      if (has_mean) {
        MatrixXd sample_mean(sigma_obs.rows(), 1);
        MatrixXd mu_obs(sigma_obs.rows(), 1);
        for (int i = 0; i < static_cast<int>(obs_idx.size()); ++i) {
          sample_mean(i, 0) = sample_mean_vec[i];
          mu_obs(i, 0) = mu(obs_idx[i], 0);
        }
        const MatrixXd diff = sample_mean - mu_obs;
        S_centered = S_centered + diff * diff.transpose();
        d_ll_d_mu_obs = nobs * sigma_inv * diff;
      }

      const double p = static_cast<double>(sample_cov.rows());
      const double logdet_sigma =
        2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();
      const double trace_Sc = (S_centered.cwiseProduct(sigma_inv)).sum();
      const double ll_pat = -0.5 * nobs * (p * std::log(2.0 * M_PI) + logdet_sigma + trace_Sc);
      if (!R_finite(ll_pat)) return false;
      ll_out += ll_pat;

      const MatrixXd d_sigma_obs = 0.5 * nobs * (sigma_inv * S_centered * sigma_inv - sigma_inv);
      const MatrixXd M = lambda_obs.transpose() * d_sigma_obs * lambda_obs;
      MatrixXd d_lambda_obs = 2.0 * d_sigma_obs * lambda_obs * veta;
      if (has_mean) {
        d_lambda_obs = d_lambda_obs + d_ll_d_mu_obs * eta_mean.transpose();
      }
      for (int i = 0; i < static_cast<int>(obs_idx.size()); ++i) {
        const int row = obs_idx[i];
        d_ll_d_lambda.row(row) += d_lambda_obs.row(i);
        if (has_mean) d_ll_d_nu(row, 0) += d_ll_d_mu_obs(i, 0);
        for (int j = 0; j < static_cast<int>(obs_idx.size()); ++j) {
          d_ll_d_theta(row, obs_idx[j]) += d_sigma_obs(i, j);
        }
      }

      d_ll_d_psi += has_beta ? Ainv.transpose() * M * Ainv : M;
      if (has_beta) {
        MatrixXd d_beta_pat = 2.0 * Ainv.transpose() * M * veta;
        if (has_mean) {
          d_ll_d_alpha += Ainv.transpose() * lambda_obs.transpose() * d_ll_d_mu_obs;
          d_beta_pat = d_beta_pat +
            Ainv.transpose() * lambda_obs.transpose() * d_ll_d_mu_obs * eta_mean.transpose();
        }
        d_ll_d_beta += d_beta_pat;
      } else if (has_mean) {
        d_ll_d_alpha += lambda_obs.transpose() * d_ll_d_mu_obs;
      }
    }
  } else {
    Eigen::LLT<MatrixXd> llt(sigma);
    if (llt.info() != Eigen::Success) return false;

    const MatrixXd sample_cov = as<MatrixXd>(group["sample_cov"]);
    const double nobs = as<double>(group["nobs"]);
    const double p    = static_cast<double>(sample_cov.rows());

    MatrixXd sample_mean = MatrixXd::Zero(lambda.rows(), 1);
    if (has_mean) sample_mean = as<MatrixXd>(group["sample_mean"]);

    const MatrixXd sigma_inv = llt.solve(MatrixXd::Identity(sigma.rows(), sigma.cols()));
    if (!sigma_inv.allFinite()) return false;

    MatrixXd S_centered = sample_cov;
    MatrixXd d_ll_d_mu  = MatrixXd::Zero(lambda.rows(), 1);
    if (has_mean) {
      const MatrixXd diff = sample_mean - mu;
      S_centered = S_centered + diff * diff.transpose();
      d_ll_d_mu  = nobs * sigma_inv * diff;
    }

    const double logdet_sigma =
      2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();
    const double trace_Sc = (S_centered.cwiseProduct(sigma_inv)).sum();
    ll_out = -0.5 * nobs * (p * std::log(2.0 * M_PI) + logdet_sigma + trace_Sc);
    if (!R_finite(ll_out)) return false;

    const MatrixXd d_sigma = 0.5 * nobs * (sigma_inv * S_centered * sigma_inv - sigma_inv);
    const MatrixXd M       = lambda.transpose() * d_sigma * lambda;
    d_ll_d_lambda          = 2.0 * d_sigma * lambda * veta;
    d_ll_d_theta           = d_sigma;
    d_ll_d_psi             = has_beta ? Ainv.transpose() * M * Ainv : M;

    if (has_beta) {
      d_ll_d_beta = 2.0 * Ainv.transpose() * M * veta;
    }
    if (has_mean) {
      d_ll_d_lambda = d_ll_d_lambda + d_ll_d_mu * eta_mean.transpose();
      d_ll_d_nu     = d_ll_d_mu;
      if (has_beta) {
        d_ll_d_alpha = Ainv.transpose() * lambda.transpose() * d_ll_d_mu;
        d_ll_d_beta  = d_ll_d_beta +
          Ainv.transpose() * lambda.transpose() * d_ll_d_mu * eta_mean.transpose();
      } else {
        d_ll_d_alpha = lambda.transpose() * d_ll_d_mu;
      }
    }
  }

  grad_out.fill(0.0);
  auto accumulate = [&grad_out](const List& spec, const MatrixXd& gmat) {
    IntegerVector free_x_idx   = spec["free_x_idx"];
    IntegerVector free_pos_idx = spec["free_pos_idx"];
    const int nrow = as<int>(spec["nrow"]);
    for (int k = 0; k < free_x_idx.size(); ++k) {
      const int pos = free_pos_idx[k] - 1;
      grad_out[free_x_idx[k] - 1] += gmat(pos % nrow, pos / nrow);
    }
  };

  accumulate(matrices["lambda"], d_ll_d_lambda);
  accumulate(matrices["theta"],  d_ll_d_theta);
  accumulate(matrices["psi"],    d_ll_d_psi);
  if (has_beta) accumulate(matrices["beta"], d_ll_d_beta);
  if (has_mean) {
    accumulate(matrices["nu"],    d_ll_d_nu);
    accumulate(matrices["alpha"], d_ll_d_alpha);
  }

  return true;
}

double cpp_lisrel_loglik(List model, NumericVector x_free);
// Forward declaration (defined after TwoLevelGroupData_ struct below)
bool twolevel_loglik_and_grad_impl(const List& model,
                                    const std::vector<double>& xf, int np,
                                    double& total_ll, std::vector<double>& gv);

// [[Rcpp::export]]
List cpp_lisrel_loglik_and_grad(List model, NumericVector x_free) {
  CharacterVector type_v = model["type"];
  const std::string type = as<std::string>(type_v[0]);
  if (type == "lisrel_ml_twolevel") {
    const std::vector<double> xf(x_free.begin(), x_free.end());
    const int np = (int)xf.size();
    double total_ll = 0.0;
    std::vector<double> gv(np, 0.0);
    if (!twolevel_loglik_and_grad_impl(model, xf, np, total_ll, gv)) {
      return List::create(Named("loglik") = total_ll,
                          Named("grad")   = NumericVector(np, NA_REAL));
    }
    NumericVector grad(gv.begin(), gv.end());
    return List::create(Named("loglik") = total_ll, Named("grad") = grad);
  }

  List groups = model["groups"];
  double total_ll = 0.0;
  NumericVector total_grad(x_free.size(), 0.0);

  for (int g = 0; g < groups.size(); ++g) {
    double ll_g = 0.0;
    NumericVector grad_g(x_free.size(), 0.0);
    if (!lisrel_loglik_and_grad_group_impl(groups[g], x_free, x_free.size(), ll_g, grad_g)) {
      return List::create(
        Named("loglik") = -1e40,
        Named("grad")   = NumericVector(x_free.size(), NA_REAL)
      );
    }
    total_ll   += ll_g;
    total_grad  = total_grad + grad_g;
  }

  return List::create(Named("loglik") = total_ll, Named("grad") = total_grad);
}

// [[Rcpp::export]]
double cpp_lisrel_loglik(List model, NumericVector x_free) {
  CharacterVector type_v = model["type"];
  const std::string type = as<std::string>(type_v[0]);
  if (type == "lisrel_ml_twolevel") {
    List groups = model["groups"];
    double total = 0.0;
    for (int g = 0; g < groups.size(); ++g) {
      const double ll_g = lisrel_loglik_twolevel_group_impl(groups[g], x_free);
      if (!R_finite(ll_g)) return -1e40;
      total += ll_g;
    }
    return total;
  }

  List groups = model["groups"];
  double total = 0.0;
  auto subset_square_local = [](const MatrixXd& x, const std::vector<int>& idx) {
    MatrixXd out(idx.size(), idx.size());
    for (int j = 0; j < static_cast<int>(idx.size()); ++j)
      for (int i = 0; i < static_cast<int>(idx.size()); ++i)
        out(i, j) = x(idx[i], idx[j]);
    return out;
  };

  for (int g = 0; g < groups.size(); ++g) {
    const List group = groups[g];
    const MatrixXd sigma = lisrel_implied_cov_group_impl(group, x_free);
    if (sigma.size() == 0) {
      return -1e40;
    }

    LogicalVector has_missing_v = group["has_missing"];
    const bool has_missing = has_missing_v[0];
    const bool has_mean = as<bool>(group["has_mean"]);
    MatrixXd mu = MatrixXd::Zero(sigma.rows(), 1);
    if (has_mean) {
      mu = lisrel_implied_mean_group_impl(group, x_free);
      if (mu.size() == 0) {
        return -1e40;
      }
    }

    if (has_missing) {
      const List missing = group["missing"];
      for (int m = 0; m < missing.size(); ++m) {
        const List rec = missing[m];
        const MatrixXd sample_cov = missing_pattern_cov_from_record(rec);
        NumericVector sample_mean_vec = has_mean ? as<NumericVector>(rec["MY"]) : NumericVector(0);
        LogicalVector var_idx = rec["var_idx"];
        IntegerVector freq_v = rec["freq"];
        const double nobs = freq_v[0];

        std::vector<int> obs_idx;
        obs_idx.reserve(var_idx.size());
        for (int i = 0; i < var_idx.size(); ++i) {
          if (var_idx[i]) obs_idx.push_back(i);
        }

        const MatrixXd sigma_obs = subset_square_local(sigma, obs_idx);
        Eigen::LLT<MatrixXd> llt(sigma_obs);
        if (llt.info() != Eigen::Success) {
          return -1e40;
        }
        const MatrixXd sigma_inv = llt.solve(MatrixXd::Identity(sigma_obs.rows(), sigma_obs.cols()));
        if (llt.info() != Eigen::Success || !sigma_inv.allFinite()) {
          return -1e40;
        }
        const double p = static_cast<double>(sample_cov.rows());
        const double logdet_sigma = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();
        const double trace_term = (sample_cov.cwiseProduct(sigma_inv)).sum();
        double mean_term = 0.0;
        if (has_mean) {
          MatrixXd sample_mean(sigma_obs.rows(), 1);
          MatrixXd mu_obs(sigma_obs.rows(), 1);
          for (int i = 0; i < static_cast<int>(obs_idx.size()); ++i) {
            sample_mean(i, 0) = sample_mean_vec[i];
            mu_obs(i, 0) = mu(obs_idx[i], 0);
          }
          const MatrixXd diff = sample_mean - mu_obs;
          mean_term = (diff.transpose() * sigma_inv * diff)(0, 0);
        }
        const double out = -0.5 * nobs * (p * std::log(2.0 * M_PI) + logdet_sigma + trace_term + mean_term);
        if (!R_finite(out)) {
          return -1e40;
        }
        total += out;
      }
      continue;
    }

    Eigen::LLT<MatrixXd> llt(sigma);
    if (llt.info() != Eigen::Success) {
      return -1e40;
    }

    const MatrixXd sample_cov = as<MatrixXd>(group["sample_cov"]);
    const double nobs = as<double>(group["nobs"]);
    const double p = static_cast<double>(sample_cov.rows());
    MatrixXd sample_mean = MatrixXd::Zero(sample_cov.rows(), 1);
    if (has_mean) {
      sample_mean = as<MatrixXd>(group["sample_mean"]);
    }
    const MatrixXd sigma_inv = llt.solve(MatrixXd::Identity(sigma.rows(), sigma.cols()));
    if (llt.info() != Eigen::Success || !sigma_inv.allFinite()) {
      return -1e40;
    }

    const double logdet_sigma = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();
    const double trace_term = (sample_cov.cwiseProduct(sigma_inv)).sum();
    double mean_term = 0.0;
    if (has_mean) {
      const MatrixXd diff = sample_mean - mu;
      mean_term = (diff.transpose() * sigma_inv * diff)(0, 0);
    }
    const double out = -0.5 * nobs * (p * std::log(2.0 * M_PI) + logdet_sigma + trace_term + mean_term);
    if (!R_finite(out)) {
      return -1e40;
    }
    total += out;
  }

  return total;
}

// [[Rcpp::export]]
NumericVector cpp_lisrel_grad(List model, NumericVector x_free) {
  return cpp_lisrel_loglik_and_grad(model, x_free)["grad"];
}

// [[Rcpp::export]]
NumericMatrix cpp_lisrel_hessian_theta(
    List model,
    NumericVector par,
    IntegerVector transforms,
    IntegerVector cov_var_idx1,  // 1-based var param index for type-2; 0 if none
    IntegerVector cov_var_idx2,  // 1-based var param index for type-2; 0 if none
    const double h_hess = 1e-5
) {
  const int n = par.size();
  List groups = model["groups"];
  const int ng = groups.size();
  const std::vector<GcpBlockCpp> gcp_blocks = extract_gcp_blocks_cpp(model);
  const PriorMetaCpp prior_meta = extract_prior_meta_cpp(model);

  auto fg = [&](const VectorXd& pars, double& fval, VectorXd& gval) -> bool {
    std::vector<double> theta_v(n), x_vec;
    std::vector<int> tr_v(n), cvi1(n), cvi2(n);
    for (int i = 0; i < n; ++i) {
      theta_v[i] = pars[i];
      tr_v[i] = transforms[i];
      cvi1[i] = cov_var_idx1[i];
      cvi2[i] = cov_var_idx2[i];
    }
    std::vector<GcpBlockEvalCpp> gcp_eval;
    if (!theta_to_x_gcp_cpp(theta_v, tr_v, cvi1, cvi2, gcp_blocks, x_vec, &gcp_eval)) return false;
    NumericVector x_free(n);
    for (int i = 0; i < n; ++i) x_free[i] = x_vec[i];

    fval = 0.0;
    NumericVector grad_x(n, 0.0);
    for (int g = 0; g < ng; ++g) {
      double ll_g = 0.0;
      NumericVector gg(n, 0.0);
      if (!lisrel_loglik_and_grad_group_impl(groups[g], x_free, n, ll_g, gg)) {
        return false;
      }
      fval -= ll_g;
      for (int i = 0; i < n; ++i) grad_x[i] -= gg[i];
    }
    if (!R_finite(fval)) return false;

    gval.resize(n);
    gval.setZero();
    for (int i = 0; i < n; ++i) {
      double chain;
      switch (transforms[i]) {
        case 1:  chain = x_free[i]; break;
        case 2:
          if (cov_var_idx1[i] > 0) {
            const double rho = std::tanh(pars[i]);
            const double sd1 = std::sqrt(std::exp(pars[cov_var_idx1[i] - 1]));
            const double sd2 = std::sqrt(std::exp(pars[cov_var_idx2[i] - 1]));
            chain = (1.0 - rho * rho) * sd1 * sd2;
          } else {
            chain = 1.0 - x_free[i] * x_free[i];
          }
          break;
        default: chain = 1.0;
      }
      gval[i] = grad_x[i] * chain;
    }
    for (int i = 0; i < n; ++i) {
      if (transforms[i] == 2 && cov_var_idx1[i] > 0) {
        const double half_cov = 0.5 * x_free[i];
        gval[cov_var_idx1[i] - 1] += grad_x[i] * half_cov;
        gval[cov_var_idx2[i] - 1] += grad_x[i] * half_cov;
      }
    }
    for (int b = 0; b < (int)gcp_blocks.size(); ++b) {
      const auto& blk = gcp_blocks[b];
      if (gcp_eval[b].J.size() == 0) return false;
      VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        const int idx = blk.theta_idx0[k];
        if (cov_var_idx1[idx] > 0) {
          const double sd1 = std::sqrt(std::exp(pars[cov_var_idx1[idx] - 1]));
          const double sd2 = std::sqrt(std::exp(pars[cov_var_idx2[idx] - 1]));
          rho_grad[k] = grad_x[idx] * sd1 * sd2;
        } else {
          rho_grad[k] = grad_x[idx];
        }
      }
      const VectorXd theta_grad = gcp_eval[b].J.transpose() * rho_grad;
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        gval[blk.theta_idx0[k]] = theta_grad[k];
      }
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        const int idx = blk.theta_idx0[k];
        if (cov_var_idx1[idx] > 0) {
          const double half_cov = 0.5 * x_free[idx];
          gval[cov_var_idx1[idx] - 1] += grad_x[idx] * half_cov;
          gval[cov_var_idx2[idx] - 1] += grad_x[idx] * half_cov;
        }
      }
    }
    if (!prior_meta.free_id0.empty()) {
      std::vector<double> gpr;
      double lp = 0.0;
      if (!prior_lp_grad_cpp(theta_v, prior_meta, gcp_blocks, lp, &gpr, &gcp_eval)) return false;
      fval -= lp;
      for (int i = 0; i < n; ++i) gval[i] -= gpr[i];
    }
    return true;
  };

  VectorXd pars(n);
  for (int i = 0; i < n; ++i) pars[i] = par[i];

  // Forward-difference Hessian: n+1 kernel calls instead of 2n.
  // Evaluate gradient at centre once, then perturb each coordinate forward.
  double f0 = 0.0;
  VectorXd g0(n);
  bool ok0 = fg(pars, f0, g0);

  MatrixXd H_neg(n, n);
  H_neg.setZero();
  for (int j = 0; j < n; ++j) {
    VectorXd pp = pars; pp[j] += h_hess;
    double fp = 0.0;
    VectorXd gp(n);
    bool okp = fg(pp, fp, gp);
    for (int i = 0; i < n; ++i) {
      H_neg(i, j) = (ok0 && okp) ? (gp[i] - g0[i]) / h_hess : 0.0;
    }
  }
  // Symmetrize to reduce rounding asymmetry from the one-sided scheme
  H_neg = 0.5 * (H_neg + H_neg.transpose());

  return wrap(H_neg);
}

// ---------------------------------------------------------------------------
// C++ structs for thread-safe pre-extracted group data
// ---------------------------------------------------------------------------
struct MatrixSpec_ {
  std::vector<double> values;
  std::vector<int>    free_x_idx;
  std::vector<int>    free_pos_idx;
  int nrow, ncol;
};

struct GroupData_ {
  // specs index: 0=lambda, 1=theta, 2=psi, 3=beta (optional), 4=nu (opt), 5=alpha (opt)
  std::vector<MatrixSpec_> specs;
  bool        has_beta, has_mean, has_missing;
  MatrixXd    sample_cov;
  MatrixXd    sample_mean;
  double      nobs;
  struct MissingPattern_ {
    MatrixXd SY;
    VectorXd MY;
    std::vector<int> obs_idx;
    double freq;
  };
  std::vector<MissingPattern_> missing_patterns;
};

inline MatrixSpec_ extract_matrix_spec(const List& spec) {
  MatrixSpec_ ms;
  NumericVector vals = spec["values"];
  IntegerVector fxi  = spec["free_x_idx"];
  IntegerVector fpi  = spec["free_pos_idx"];
  ms.values.assign(vals.begin(), vals.end());
  ms.free_x_idx.assign(fxi.begin(), fxi.end());
  ms.free_pos_idx.assign(fpi.begin(), fpi.end());
  ms.nrow = as<int>(spec["nrow"]);
  ms.ncol = as<int>(spec["ncol"]);
  return ms;
}

inline MatrixXd reconstruct_from_spec(const MatrixSpec_& ms,
                                       const std::vector<double>& x_free) {
  std::vector<double> vals = ms.values;
  for (int k = 0; k < (int)ms.free_x_idx.size(); ++k) {
    vals[ms.free_pos_idx[k] - 1] = x_free[ms.free_x_idx[k] - 1];
  }
  MatrixXd out(ms.nrow, ms.ncol);
  for (int j = 0; j < ms.ncol; ++j)
    for (int i = 0; i < ms.nrow; ++i)
      out(i, j) = vals[i + j * ms.nrow];
  return out;
}

// spec index helpers (specs layout: [lambda, theta, psi, (beta)?, (nu, alpha)?])
inline int spec_nu_idx(bool has_beta)    { return has_beta ? 4 : 3; }
inline int spec_alpha_idx(bool has_beta) { return has_beta ? 5 : 4; }

inline MatrixXd subset_square(const MatrixXd& x, const std::vector<int>& idx) {
  const int n = static_cast<int>(idx.size());
  MatrixXd out(n, n);
  for (int j = 0; j < n; ++j)
    for (int i = 0; i < n; ++i)
      out(i, j) = x(idx[i], idx[j]);
  return out;
}

inline MatrixXd subset_rows(const MatrixXd& x, const std::vector<int>& idx) {
  const int n = static_cast<int>(idx.size());
  MatrixXd out(n, x.cols());
  for (int i = 0; i < n; ++i)
    out.row(i) = x.row(idx[i]);
  return out;
}

inline VectorXd subset_vec(const MatrixXd& x, const std::vector<int>& idx) {
  const int n = static_cast<int>(idx.size());
  VectorXd out(n);
  for (int i = 0; i < n; ++i)
    out(i) = x(idx[i], 0);
  return out;
}

inline double group_loglik_from_data(const GroupData_& gd,
                                      const std::vector<double>& x_free) {
  const MatrixXd lambda = reconstruct_from_spec(gd.specs[0], x_free);
  const MatrixXd theta  = reconstruct_from_spec(gd.specs[1], x_free);
  const MatrixXd psi    = reconstruct_from_spec(gd.specs[2], x_free);

  MatrixXd Ainv_l = MatrixXd::Identity(psi.rows(), psi.cols());
  MatrixXd eta_cov = psi;
  MatrixXd eta_mean = MatrixXd::Zero(psi.rows(), 1);
  if (gd.has_beta) {
    const MatrixXd beta = reconstruct_from_spec(gd.specs[3], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return -1e40;
    Ainv_l   = lu.inverse();
    eta_cov  = Ainv_l * psi * Ainv_l.transpose();
    if (gd.has_mean)
      eta_mean = Ainv_l * reconstruct_from_spec(gd.specs[spec_alpha_idx(true)], x_free);
  } else if (gd.has_mean) {
    eta_mean = reconstruct_from_spec(gd.specs[spec_alpha_idx(false)], x_free);
  }

  const MatrixXd sigma = lambda * eta_cov * lambda.transpose() + theta;
  MatrixXd nu = MatrixXd::Zero(lambda.rows(), 1);
  MatrixXd mu = MatrixXd::Zero(lambda.rows(), 1);
  if (gd.has_mean) {
    nu = reconstruct_from_spec(gd.specs[spec_nu_idx(gd.has_beta)], x_free);
    mu = nu + lambda * eta_mean;
  }

  double ll_total = 0.0;
  if (gd.has_missing) {
    for (const auto& pat : gd.missing_patterns) {
      const MatrixXd sigma_obs = subset_square(sigma, pat.obs_idx);
      Eigen::LLT<MatrixXd> llt(sigma_obs);
      if (llt.info() != Eigen::Success) return -1e40;
      const int p_obs = sigma_obs.rows();
      const MatrixXd si = llt.solve(MatrixXd::Identity(p_obs, p_obs));
      const double logdet = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();
      MatrixXd S_c = pat.SY;
      if (gd.has_mean) {
        const VectorXd mu_obs = subset_vec(mu, pat.obs_idx);
        const VectorXd dff = pat.MY - mu_obs;
        S_c += dff * dff.transpose();
      }
      const double tr = (S_c.cwiseProduct(si)).sum();
      ll_total += -0.5 * pat.freq * (p_obs * std::log(2.0 * M_PI) + logdet + tr);
    }
  } else {
    Eigen::LLT<MatrixXd> llt(sigma);
    if (llt.info() != Eigen::Success) return -1e40;

    const double p      = static_cast<double>(gd.sample_cov.rows());
    const MatrixXd si   = llt.solve(MatrixXd::Identity((int)p, (int)p));
    const double logdet = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();

    MatrixXd S_c = gd.sample_cov;
    if (gd.has_mean) {
      const MatrixXd dff = gd.sample_mean - mu;
      S_c += dff * dff.transpose();
    }

    const double tr = (S_c.cwiseProduct(si)).sum();
    ll_total = -0.5 * gd.nobs * (p * std::log(2.0 * M_PI) + logdet + tr);
  }
  return R_finite(ll_total) ? ll_total : -1e40;
}

// Loglik + gradient (in x-space) from pre-extracted group data
// Returns false on failure; accumulates into fval and gval (negated: for minimisation)
inline bool group_loglik_grad_from_data(const GroupData_& gd,
                                         const std::vector<double>& x_free,
                                         int npar,
                                         double& fval,
                                         std::vector<double>& gval) {
  const MatrixXd lambda  = reconstruct_from_spec(gd.specs[0], x_free);
  const MatrixXd theta_m = reconstruct_from_spec(gd.specs[1], x_free);
  const MatrixXd psi     = reconstruct_from_spec(gd.specs[2], x_free);

  MatrixXd Ainv = MatrixXd::Identity(psi.rows(), psi.cols());
  MatrixXd eta_cov  = psi;
  MatrixXd eta_mean = MatrixXd::Zero(psi.rows(), 1);
  if (gd.has_beta) {
    const MatrixXd beta = reconstruct_from_spec(gd.specs[3], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return false;
    Ainv     = lu.inverse();
    eta_cov  = Ainv * psi * Ainv.transpose();
    if (gd.has_mean)
      eta_mean = Ainv * reconstruct_from_spec(gd.specs[spec_alpha_idx(true)], x_free);
  } else if (gd.has_mean) {
    eta_mean = reconstruct_from_spec(gd.specs[spec_alpha_idx(false)], x_free);
  }

  const MatrixXd sigma = lambda * eta_cov * lambda.transpose() + theta_m;
  MatrixXd nu = MatrixXd::Zero(lambda.rows(), 1);
  MatrixXd mu = MatrixXd::Zero(lambda.rows(), 1);
  if (gd.has_mean) {
    nu = reconstruct_from_spec(gd.specs[spec_nu_idx(gd.has_beta)], x_free);
    mu = nu + lambda * eta_mean;
  }

  MatrixXd dL_dLambda = MatrixXd::Zero(lambda.rows(), lambda.cols());
  MatrixXd dL_dTheta  = MatrixXd::Zero(theta_m.rows(), theta_m.cols());
  MatrixXd dL_dPsi    = MatrixXd::Zero(psi.rows(), psi.cols());
  MatrixXd dL_dBeta   = gd.has_beta ? MatrixXd::Zero(psi.rows(), psi.cols()) : MatrixXd();
  MatrixXd dL_dNu     = gd.has_mean ? MatrixXd::Zero(lambda.rows(), 1) : MatrixXd();
  MatrixXd dL_dAlpha  = gd.has_mean ? MatrixXd::Zero(psi.rows(), 1) : MatrixXd();

  if (gd.has_missing) {
    for (const auto& pat : gd.missing_patterns) {
      const MatrixXd sigma_obs = subset_square(sigma, pat.obs_idx);
      const MatrixXd lambda_obs = subset_rows(lambda, pat.obs_idx);
      const MatrixXd theta_obs = subset_square(theta_m, pat.obs_idx);
      const VectorXd mu_obs = gd.has_mean ? subset_vec(mu, pat.obs_idx) : VectorXd::Zero(0);

      Eigen::LLT<MatrixXd> llt(sigma_obs);
      if (llt.info() != Eigen::Success) return false;
      const int p_obs = sigma_obs.rows();
      const MatrixXd si = llt.solve(MatrixXd::Identity(p_obs, p_obs));
      const double logdet = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();

      MatrixXd S_c = pat.SY;
      MatrixXd d_ll_d_mu = MatrixXd::Zero(p_obs, 1);
      if (gd.has_mean) {
        const VectorXd dff = pat.MY - mu_obs;
        S_c += dff * dff.transpose();
        d_ll_d_mu = pat.freq * si * dff;
      }

      const double tr = (S_c.cwiseProduct(si)).sum();
      const double ll = -0.5 * pat.freq * (p_obs * std::log(2.0 * M_PI) + logdet + tr);
      if (!R_finite(ll)) return false;
      fval -= ll;

      const MatrixXd dSigma_obs = 0.5 * pat.freq * (si * S_c * si - si);
      const MatrixXd M = lambda_obs.transpose() * dSigma_obs * lambda_obs;
      MatrixXd dLambda_obs = 2.0 * dSigma_obs * lambda_obs * eta_cov;
      if (gd.has_mean) {
        dLambda_obs += d_ll_d_mu * eta_mean.transpose();
        for (int i = 0; i < p_obs; ++i) {
          const int row = pat.obs_idx[i];
          dL_dNu(row, 0) += d_ll_d_mu(i, 0);
        }
      }
      for (int i = 0; i < p_obs; ++i) {
        const int row = pat.obs_idx[i];
        dL_dLambda.row(row) += dLambda_obs.row(i);
        for (int j = 0; j < p_obs; ++j) {
          const int col = pat.obs_idx[j];
          dL_dTheta(row, col) += dSigma_obs(i, j);
        }
      }
      dL_dPsi += gd.has_beta ? Ainv.transpose() * M * Ainv : M;
      if (gd.has_beta) {
        MatrixXd dBeta_pat = 2.0 * Ainv.transpose() * M * eta_cov;
        if (gd.has_mean) {
          dL_dAlpha += Ainv.transpose() * (lambda_obs.transpose() * d_ll_d_mu);
          dBeta_pat +=
            Ainv.transpose() * (lambda_obs.transpose() * d_ll_d_mu) * eta_mean.transpose();
        }
        dL_dBeta += dBeta_pat;
      } else if (gd.has_mean) {
        dL_dAlpha += lambda_obs.transpose() * d_ll_d_mu;
      }
    }
  } else {
    Eigen::LLT<MatrixXd> llt(sigma);
    if (llt.info() != Eigen::Success) return false;

    const double p    = static_cast<double>(gd.sample_cov.rows());
    const int    pi   = (int)p;
    const MatrixXd si = llt.solve(MatrixXd::Identity(pi, pi));
    const double logdet = 2.0 * llt.matrixL().toDenseMatrix().diagonal().array().log().sum();

    MatrixXd S_c       = gd.sample_cov;
    MatrixXd d_ll_d_mu = MatrixXd::Zero(pi, 1);
    if (gd.has_mean) {
      const MatrixXd dff = gd.sample_mean - mu;
      S_c       += dff * dff.transpose();
      d_ll_d_mu  = gd.nobs * si * dff;
    }

    const double tr = (S_c.cwiseProduct(si)).sum();
    const double ll = -0.5 * gd.nobs * (p * std::log(2.0 * M_PI) + logdet + tr);
    if (!R_finite(ll)) return false;
    fval -= ll;

    const MatrixXd dL_dSigma = 0.5 * gd.nobs * (si * S_c * si - si);
    const MatrixXd M         = lambda.transpose() * dL_dSigma * lambda;
    dL_dLambda               = 2.0 * dL_dSigma * lambda * eta_cov;
    dL_dTheta                = dL_dSigma;
    dL_dPsi                  = gd.has_beta ? Ainv.transpose() * M * Ainv : M;

    if (gd.has_beta)
      dL_dBeta = 2.0 * Ainv.transpose() * M * eta_cov;

    if (gd.has_mean) {
      dL_dLambda = dL_dLambda + d_ll_d_mu * eta_mean.transpose();
      dL_dNu     = d_ll_d_mu;
      if (gd.has_beta) {
        dL_dAlpha = Ainv.transpose() * (lambda.transpose() * d_ll_d_mu);
        dL_dBeta  = dL_dBeta +
          Ainv.transpose() * (lambda.transpose() * d_ll_d_mu) * eta_mean.transpose();
      } else {
        dL_dAlpha = lambda.transpose() * d_ll_d_mu;
      }
    }
  }

  // Accumulate into gval (negated for minimisation: gval[xi] -= grad_ll[xi])
  auto accum = [&](const MatrixXd& dL_dM, const MatrixSpec_& ms) {
    const int nr = ms.nrow;
    for (int k = 0; k < (int)ms.free_x_idx.size(); ++k) {
      const int xi  = ms.free_x_idx[k]  - 1;
      const int pi2 = ms.free_pos_idx[k] - 1;
      const int row = pi2 % nr;
      const int col = pi2 / nr;
      if (xi < npar) gval[xi] -= dL_dM(row, col);
    }
  };
  accum(dL_dLambda, gd.specs[0]);
  accum(dL_dTheta,  gd.specs[1]);
  accum(dL_dPsi,    gd.specs[2]);
  if (gd.has_beta)  accum(dL_dBeta,  gd.specs[3]);
  if (gd.has_mean) {
    accum(dL_dNu,    gd.specs[spec_nu_idx(gd.has_beta)]);
    accum(dL_dAlpha, gd.specs[spec_alpha_idx(gd.has_beta)]);
  }
  return true;
}

// Extract all group data into C++ structs
inline std::vector<GroupData_> extract_groups(const List& model) {
  const List groups = model["groups"];
  const int ng = groups.size();
  std::vector<GroupData_> gds(ng);
  for (int g = 0; g < ng; ++g) {
    const List& grp = groups[g];
    GroupData_& gd = gds[g];
    gd.has_beta = as<bool>(grp["has_beta"]);
    gd.has_mean = as<bool>(grp["has_mean"]);
    LogicalVector has_missing_v = grp["has_missing"];
    gd.has_missing = has_missing_v[0];
    gd.nobs     = gd.has_missing ? 0.0 : as<double>(grp["nobs"]);
    if (!gd.has_missing) {
      gd.sample_cov = as<MatrixXd>(grp["sample_cov"]);
      if (gd.has_mean)
        gd.sample_mean = as<MatrixXd>(grp["sample_mean"]);
    } else {
      const List miss = grp["missing"];
      gd.missing_patterns.resize(miss.size());
      for (int i = 0; i < miss.size(); ++i) {
        const List rec = miss[i];
        auto& pat = gd.missing_patterns[i];
        pat.SY = missing_pattern_cov_from_record(rec);
        NumericVector my = rec["MY"];
        pat.MY.resize(my.size());
        for (int j = 0; j < my.size(); ++j) pat.MY(j) = my[j];
        LogicalVector var_idx = rec["var_idx"];
        pat.obs_idx.reserve(var_idx.size());
        for (int j = 0; j < var_idx.size(); ++j) {
          if (var_idx[j]) pat.obs_idx.push_back(j);
        }
        IntegerVector freq_v = rec["freq"];
        pat.freq = freq_v[0];
        gd.nobs += pat.freq;
      }
    }

    const List mats = grp["matrices"];
    gd.specs.resize(gd.has_beta ? (gd.has_mean ? 6 : 4) : (gd.has_mean ? 5 : 3));
    gd.specs[0] = extract_matrix_spec(mats["lambda"]);
    gd.specs[1] = extract_matrix_spec(mats["theta"]);
    gd.specs[2] = extract_matrix_spec(mats["psi"]);
    if (gd.has_beta) gd.specs[3] = extract_matrix_spec(mats["beta"]);
    if (gd.has_mean) {
      gd.specs[gd.has_beta ? 4 : 3] = extract_matrix_spec(mats["nu"]);
      gd.specs[gd.has_beta ? 5 : 4] = extract_matrix_spec(mats["alpha"]);
    }
  }
  return gds;
}

// ---------------------------------------------------------------------------
// Twolevel group data structures for thread-safe VB correction
// ---------------------------------------------------------------------------

struct BlockSpec_ {
  std::vector<MatrixSpec_> specs;  // [lambda,theta,psi,(beta)?,(nu)?,(alpha)?]
  bool has_beta, has_mean;
};

struct ClusterLpData_ {
  std::vector<int> both_idx, within_idx, between_idx;  // 1-based
  std::vector<int> ov_idx_w, ov_idx_b;                 // 1-based
  int nobs, nclusters;
  std::vector<int> cluster_size, cluster_sizes, cluster_size_ns;
  bool has_x;
};

struct TwoLevelGroupData_ {
  BlockSpec_            within, between;
  MatrixXd              Sigma_W;
  std::vector<MatrixXd> cov_d;
  std::vector<VectorXd> mean_d;
  double                loglik_x;
  ClusterLpData_        lp;
};

inline BlockSpec_ extract_block_spec(const List& block) {
  BlockSpec_ bs;
  bs.has_beta = as<bool>(block["has_beta"]);
  bs.has_mean = as<bool>(block["has_mean"]);
  const List mats = block["matrices"];
  const int nspc = bs.has_beta ? (bs.has_mean ? 6 : 4) : (bs.has_mean ? 5 : 3);
  bs.specs.resize(nspc);
  bs.specs[0] = extract_matrix_spec(mats["lambda"]);
  bs.specs[1] = extract_matrix_spec(mats["theta"]);
  bs.specs[2] = extract_matrix_spec(mats["psi"]);
  if (bs.has_beta)  bs.specs[3] = extract_matrix_spec(mats["beta"]);
  if (bs.has_mean) {
    bs.specs[bs.has_beta ? 4 : 3] = extract_matrix_spec(mats["nu"]);
    bs.specs[bs.has_beta ? 5 : 4] = extract_matrix_spec(mats["alpha"]);
  }
  return bs;
}

inline MatrixXd block_implied_cov(const BlockSpec_& bs,
                                   const std::vector<double>& x_free) {
  const MatrixXd lambda = reconstruct_from_spec(bs.specs[0], x_free);
  const MatrixXd theta  = reconstruct_from_spec(bs.specs[1], x_free);
  const MatrixXd psi    = reconstruct_from_spec(bs.specs[2], x_free);
  MatrixXd eta_cov = psi;
  if (bs.has_beta) {
    const MatrixXd beta = reconstruct_from_spec(bs.specs[3], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return MatrixXd();
    const MatrixXd Ainv = lu.inverse();
    eta_cov = Ainv * psi * Ainv.transpose();
  }
  return lambda * eta_cov * lambda.transpose() + theta;
}

inline MatrixXd block_implied_mean(const BlockSpec_& bs,
                                    const std::vector<double>& x_free) {
  const MatrixXd lambda = reconstruct_from_spec(bs.specs[0], x_free);
  if (!bs.has_mean) return MatrixXd::Zero(lambda.rows(), 1);
  const int nu_idx    = bs.has_beta ? 4 : 3;
  const int alpha_idx = bs.has_beta ? 5 : 4;
  const MatrixXd nu    = reconstruct_from_spec(bs.specs[nu_idx],    x_free);
  const MatrixXd alpha = reconstruct_from_spec(bs.specs[alpha_idx], x_free);
  if (bs.has_beta) {
    const MatrixXd beta = reconstruct_from_spec(bs.specs[3], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return MatrixXd();
    return nu + lambda * lu.inverse() * alpha;
  }
  return nu + lambda * alpha;
}

inline bool build_twolevel_implied_cpp(const TwoLevelGroupData_& gd,
                                        const std::vector<double>& x_free,
                                        TwoLevelImplied_& out) {
  const MatrixXd SigmaW = block_implied_cov(gd.within,  x_free);
  const MatrixXd SigmaB = block_implied_cov(gd.between, x_free);
  if (SigmaW.size() == 0 || SigmaB.size() == 0) return false;
  MatrixXd MuWm = block_implied_mean(gd.within,  x_free);
  MatrixXd MuBm = block_implied_mean(gd.between, x_free);
  if (MuWm.size() == 0 || MuBm.size() == 0) return false;

  const auto& lp = gd.lp;
  int p_tilde = 0;
  for (int i : lp.ov_idx_w) p_tilde = std::max(p_tilde, i);
  for (int i : lp.ov_idx_b) p_tilde = std::max(p_tilde, i);

  MatrixXd SigmaW_tilde = MatrixXd::Zero(p_tilde, p_tilde);
  MatrixXd SigmaB_tilde = MatrixXd::Zero(p_tilde, p_tilde);
  VectorXd MuW_tilde    = VectorXd::Zero(p_tilde);
  VectorXd MuB_tilde    = VectorXd::Zero(p_tilde);

  for (int j = 0; j < (int)lp.ov_idx_w.size(); ++j)
    for (int i = 0; i < (int)lp.ov_idx_w.size(); ++i)
      SigmaW_tilde(lp.ov_idx_w[i]-1, lp.ov_idx_w[j]-1) = SigmaW(i, j);
  for (int j = 0; j < (int)lp.ov_idx_b.size(); ++j)
    for (int i = 0; i < (int)lp.ov_idx_b.size(); ++i)
      SigmaB_tilde(lp.ov_idx_b[i]-1, lp.ov_idx_b[j]-1) = SigmaB(i, j);
  for (int i = 0; i < (int)lp.ov_idx_w.size(); ++i)
    MuW_tilde(lp.ov_idx_w[i]-1) = MuWm(i, 0);
  for (int i = 0; i < (int)lp.ov_idx_b.size(); ++i)
    MuB_tilde(lp.ov_idx_b[i]-1) = MuBm(i, 0);

  VectorXd MuWB_tilde = VectorXd::Zero(p_tilde);
  for (int idx : lp.within_idx)  MuWB_tilde(idx-1) = MuW_tilde(idx-1);
  for (int idx : lp.both_idx) {
    MuWB_tilde(idx-1) = MuB_tilde(idx-1) + MuW_tilde(idx-1);
    MuW_tilde(idx-1)  = 0.0;
    MuB_tilde(idx-1)  = MuWB_tilde(idx-1);
  }
  for (int idx : lp.within_idx)  MuB_tilde(idx-1) = 0.0;

  if (!lp.between_idx.empty()) {
    std::vector<bool> is_between(p_tilde, false);
    for (int i : lp.between_idx) is_between[i-1] = true;
    std::vector<int> not_between;
    not_between.reserve(p_tilde - (int)lp.between_idx.size());
    for (int i = 0; i < p_tilde; ++i) if (!is_between[i]) not_between.push_back(i);

    const int nz = (int)lp.between_idx.size();
    const int ny = (int)not_between.size();
    out.mu_z.resize(nz);
    for (int i = 0; i < nz; ++i) out.mu_z(i) = MuB_tilde(lp.between_idx[i]-1);
    out.mu_y.resize(ny);
    for (int i = 0; i < ny; ++i) out.mu_y(i) = MuWB_tilde(not_between[i]);

    out.sigma_zz.resize(nz, nz);
    for (int j = 0; j < nz; ++j)
      for (int i = 0; i < nz; ++i)
        out.sigma_zz(i, j) = SigmaB_tilde(lp.between_idx[i]-1, lp.between_idx[j]-1);
    out.sigma_yz.resize(ny, nz);
    out.sigma_b.resize(ny, ny);
    out.sigma_w.resize(ny, ny);
    for (int j = 0; j < nz; ++j)
      for (int i = 0; i < ny; ++i)
        out.sigma_yz(i, j) = SigmaB_tilde(not_between[i], lp.between_idx[j]-1);
    for (int j = 0; j < ny; ++j)
      for (int i = 0; i < ny; ++i) {
        out.sigma_b(i, j) = SigmaB_tilde(not_between[i], not_between[j]);
        out.sigma_w(i, j) = SigmaW_tilde(not_between[i], not_between[j]);
      }
  } else {
    out.mu_z.resize(0);
    out.mu_y  = MuWB_tilde;
    out.sigma_zz.resize(0, 0);
    out.sigma_yz.resize(p_tilde, 0);
    out.sigma_b = SigmaB_tilde;
    out.sigma_w = SigmaW_tilde;
  }
  return out.sigma_w.rows() > 0;
}

// [[Rcpp::export]]
List cpp_lisrel_twolevel_implied_blocks(List model, NumericVector x_free) {
  CharacterVector type_v = model["type"];
  const std::string type = as<std::string>(type_v[0]);
  if (type != "lisrel_ml_twolevel") {
    stop("Native two-level implied blocks require a lisrel_ml_twolevel backend.");
  }

  const std::vector<double> xf(x_free.begin(), x_free.end());
  List groups = model["groups"];
  const int ng = groups.size();
  List out(ng);

  for (int g = 0; g < ng; ++g) {
    const List group = groups[g];
    const BlockSpec_ within = extract_block_spec(group["within"]);
    const BlockSpec_ between = extract_block_spec(group["between"]);

    const MatrixXd sigma_w = block_implied_cov(within, xf);
    const MatrixXd sigma_b = block_implied_cov(between, xf);
    if (sigma_w.size() == 0 || sigma_b.size() == 0) {
      stop("Native two-level implied covariance failed to invert I-Beta.");
    }

    const MatrixXd mu_w = block_implied_mean(within, xf);
    const MatrixXd mu_b = block_implied_mean(between, xf);
    if (mu_w.size() == 0 || mu_b.size() == 0) {
      stop("Native two-level implied mean failed to invert I-Beta.");
    }

    out[g] = List::create(
      Named("within") = List::create(
        Named("cov") = wrap(sigma_w),
        Named("mean") = wrap(mu_w)
      ),
      Named("between") = List::create(
        Named("cov") = wrap(sigma_b),
        Named("mean") = wrap(mu_b)
      )
    );
  }

  return out;
}

inline double twolevel_loglik_cpp(const TwoLevelGroupData_& gd,
                                   const std::vector<double>& x_free) {
  TwoLevelImplied_ imp;
  if (!build_twolevel_implied_cpp(gd, x_free, imp)) return -1e40;

  const auto& lp = gd.lp;
  MatrixXd S_PW = gd.Sigma_W;
  if (!lp.between_idx.empty()) {
    std::vector<bool> is_between(S_PW.rows(), false);
    for (int i : lp.between_idx) is_between[i-1] = true;
    std::vector<int> not_between;
    not_between.reserve(S_PW.rows() - (int)lp.between_idx.size());
    for (int i = 0; i < S_PW.rows(); ++i) if (!is_between[i]) not_between.push_back(i);
    MatrixXd tmp((int)not_between.size(), (int)not_between.size());
    for (int j = 0; j < (int)not_between.size(); ++j)
      for (int i = 0; i < (int)not_between.size(); ++i)
        tmp(i, j) = S_PW(not_between[i], not_between[j]);
    S_PW = tmp;
  }

  Eigen::LLT<MatrixXd> llt_w(imp.sigma_w);
  if (llt_w.info() != Eigen::Success) return -1e40;
  const MatrixXd sigma_w_inv =
    llt_w.solve(MatrixXd::Identity(imp.sigma_w.rows(), imp.sigma_w.cols()));
  if (!sigma_w_inv.allFinite()) return -1e40;
  const double sigma_w_logdet =
    2.0 * llt_w.matrixL().toDenseMatrix().diagonal().array().log().sum();

  MatrixXd sigma_zz_inv, sigma_yz_zi, sigma_zi_zy;
  double sigma_zz_logdet = 0.0;
  MatrixXd sigma_b_z = imp.sigma_b;
  if (!lp.between_idx.empty()) {
    Eigen::LLT<MatrixXd> llt_zz(imp.sigma_zz);
    if (llt_zz.info() != Eigen::Success) return -1e40;
    sigma_zz_inv =
      llt_zz.solve(MatrixXd::Identity(imp.sigma_zz.rows(), imp.sigma_zz.cols()));
    if (!sigma_zz_inv.allFinite()) return -1e40;
    sigma_zz_logdet =
      2.0 * llt_zz.matrixL().toDenseMatrix().diagonal().array().log().sum();
    sigma_yz_zi = imp.sigma_yz * sigma_zz_inv;
    sigma_zi_zy = sigma_yz_zi.transpose();
    sigma_b_z   = imp.sigma_b - imp.sigma_yz * sigma_zi_zy;
  }

  VectorXd mu_concat((int)(imp.mu_z.size() + imp.mu_y.size()));
  for (int i = 0; i < (int)imp.mu_z.size(); ++i)
    mu_concat(i) = imp.mu_z(i);
  for (int i = 0; i < (int)imp.mu_y.size(); ++i)
    mu_concat((int)imp.mu_z.size() + i) = imp.mu_y(i);

  double sum_L = 0.0, sum_B = 0.0;
  for (int clz = 0; clz < (int)gd.cov_d.size(); ++clz) {
    const int nj = lp.cluster_sizes[clz];
    const VectorXd diff = gd.mean_d[clz] - mu_concat;
    const MatrixXd Y2Yc = gd.cov_d[clz] + diff * diff.transpose();

    MatrixXd Y2Yc_yy;
    double q_zz = 0.0, q_yz = 0.0;
    if (!lp.between_idx.empty()) {
      const int nz = (int)lp.between_idx.size();
      const MatrixXd Y2Yc_zz = Y2Yc.topLeftCorner(nz, nz);
      const MatrixXd Y2Yc_yz_m = Y2Yc.bottomLeftCorner(Y2Yc.rows() - nz, nz);
      Y2Yc_yy = Y2Yc.bottomRightCorner(Y2Yc.rows() - nz, Y2Yc.cols() - nz);
      const MatrixXd sigma_j = nj * sigma_b_z + imp.sigma_w;
      Eigen::LLT<MatrixXd> llt_j(sigma_j);
      if (llt_j.info() != Eigen::Success) return -1e40;
      const MatrixXd sigma_j_inv =
        llt_j.solve(MatrixXd::Identity(sigma_j.rows(), sigma_j.cols()));
      if (!sigma_j_inv.allFinite()) return -1e40;
      const double sigma_j_logdet =
        2.0 * llt_j.matrixL().toDenseMatrix().diagonal().array().log().sum();
      const MatrixXd sigma_ji_yz_zi = sigma_j_inv * sigma_yz_zi;
      const MatrixXd Vinv11 = sigma_zz_inv + nj * (sigma_zi_zy * sigma_ji_yz_zi);
      q_zz = (Vinv11.cwiseProduct(Y2Yc_zz)).sum();
      q_yz = -nj * (sigma_ji_yz_zi.cwiseProduct(Y2Yc_yz_m)).sum();
      const double q_yyc = -nj * (sigma_j_inv.cwiseProduct(Y2Yc_yy)).sum();
      sum_L += (sigma_zz_logdet + sigma_j_logdet) * lp.cluster_size_ns[clz];
      sum_B += (q_zz + 2.0 * q_yz - q_yyc) * lp.cluster_size_ns[clz];
    } else {
      Y2Yc_yy = Y2Yc;
      const MatrixXd sigma_j = nj * sigma_b_z + imp.sigma_w;
      Eigen::LLT<MatrixXd> llt_j(sigma_j);
      if (llt_j.info() != Eigen::Success) return -1e40;
      const MatrixXd sigma_j_inv =
        llt_j.solve(MatrixXd::Identity(sigma_j.rows(), sigma_j.cols()));
      if (!sigma_j_inv.allFinite()) return -1e40;
      const double sigma_j_logdet =
        2.0 * llt_j.matrixL().toDenseMatrix().diagonal().array().log().sum();
      const double q_yyc = -nj * (sigma_j_inv.cwiseProduct(Y2Yc_yy)).sum();
      sum_L += sigma_j_logdet * lp.cluster_size_ns[clz];
      sum_B += (-q_yyc) * lp.cluster_size_ns[clz];
    }
  }

  double cluster_size_m1_sum = 0.0;
  for (int c : lp.cluster_size) cluster_size_m1_sum += (c - 1.0);
  const double q_W = cluster_size_m1_sum * (sigma_w_inv.cwiseProduct(S_PW)).sum();
  const double L_W = cluster_size_m1_sum * sigma_w_logdet;

  const double LOG_2PI = std::log(2.0 * M_PI);
  const double P =
    (double)lp.nobs      * (double)(lp.both_idx.size() + lp.within_idx.size()) +
    (double)lp.nclusters * (double)lp.between_idx.size();
  double out = -0.5 * (P * LOG_2PI + sum_L + sum_B + q_W + L_W);
  if (lp.has_x) out -= gd.loglik_x;
  return R_finite(out) ? out : -1e40;
}

// ── block_lisrel_backprop ─────────────────────────────────────────────────────
// Accumulates d(ll)/d(x_free[i]) for one LISREL block.
// D_sigma : d(ll)/d(SigmaBlock) in block-variable space (pw x pw)
// D_mu    : d(ll)/d(MuBlock)    in block-variable space (pw x 1); empty if unused
inline void block_lisrel_backprop(
    const BlockSpec_&          bs,
    const MatrixXd&            D_sigma,
    const MatrixXd&            D_mu,
    const std::vector<double>& x_free,
    int                        npar,
    std::vector<double>&       grad_ll)
{
  const MatrixXd lambda = reconstruct_from_spec(bs.specs[0], x_free);
  const MatrixXd psi    = reconstruct_from_spec(bs.specs[2], x_free);

  MatrixXd Ainv     = MatrixXd::Identity(psi.rows(), psi.cols());
  MatrixXd eta_cov  = psi;
  VectorXd eta_mean = VectorXd::Zero(psi.rows());
  if (bs.has_beta) {
    const MatrixXd beta = reconstruct_from_spec(bs.specs[3], x_free);
    const MatrixXd A = MatrixXd::Identity(beta.rows(), beta.cols()) - beta;
    Eigen::FullPivLU<MatrixXd> lu(A);
    if (!lu.isInvertible()) return;
    Ainv    = lu.inverse();
    eta_cov = Ainv * psi * Ainv.transpose();
    if (bs.has_mean) {
      const MatrixXd alp = reconstruct_from_spec(bs.specs[spec_alpha_idx(true)], x_free);
      eta_mean = Ainv * alp.col(0);
    }
  } else if (bs.has_mean) {
    const MatrixXd alp = reconstruct_from_spec(bs.specs[spec_alpha_idx(false)], x_free);
    eta_mean = alp.col(0);
  }

  const MatrixXd M       = lambda.transpose() * D_sigma * lambda;
  const MatrixXd dL_dPsi = bs.has_beta ? (Ainv.transpose() * M * Ainv) : M;
  MatrixXd dL_dLambda    = 2.0 * D_sigma * lambda * eta_cov;
  MatrixXd dL_dBeta, dL_dNu, dL_dAlpha;

  const bool have_mu = bs.has_mean && D_mu.size() > 0;
  if (have_mu) {
    dL_dLambda += D_mu * eta_mean.transpose();
    dL_dNu      = D_mu;
    if (bs.has_beta) {
      const MatrixXd AtLtD = Ainv.transpose() * (lambda.transpose() * D_mu);
      dL_dAlpha = AtLtD;
      dL_dBeta  = 2.0 * Ainv.transpose() * M * eta_cov + AtLtD * eta_mean.transpose();
    } else {
      dL_dAlpha = lambda.transpose() * D_mu;
    }
  } else if (bs.has_beta) {
    dL_dBeta = 2.0 * Ainv.transpose() * M * eta_cov;
  }

  auto accum = [&](const MatrixXd& dL_dM, const MatrixSpec_& ms) {
    const int nr = ms.nrow;
    for (int k = 0; k < (int)ms.free_x_idx.size(); ++k) {
      const int xi  = ms.free_x_idx[k]  - 1;
      const int pi2 = ms.free_pos_idx[k] - 1;
      if (xi < npar) grad_ll[xi] += dL_dM(pi2 % nr, pi2 / nr);
    }
  };

  accum(dL_dLambda, bs.specs[0]);
  accum(D_sigma,    bs.specs[1]);
  accum(dL_dPsi,    bs.specs[2]);
  if (bs.has_beta  && dL_dBeta.size()  > 0) accum(dL_dBeta,  bs.specs[3]);
  if (bs.has_mean) {
    if (dL_dNu.size()    > 0) accum(dL_dNu,    bs.specs[spec_nu_idx(bs.has_beta)]);
    if (dL_dAlpha.size() > 0) accum(dL_dAlpha, bs.specs[spec_alpha_idx(bs.has_beta)]);
  }
}

// ── twolevel_loglik_grad_cpp ──────────────────────────────────────────────────
// Adds d(ll)/d(x_free[i]) to grad_ll[i]. Returns false on decomposition failure.
inline bool twolevel_loglik_grad_cpp(
    const TwoLevelGroupData_&  gd,
    const std::vector<double>& x_free,
    int                        npar,
    std::vector<double>&       grad_ll)
{
  TwoLevelImplied_ imp;
  if (!build_twolevel_implied_cpp(gd, x_free, imp)) return false;

  const auto& lp = gd.lp;
  const int   ny = imp.sigma_w.rows();
  const int   nz = (int)lp.between_idx.size();

  // Build not_between: 0-based tilde indices of non-between-only variables
  std::vector<int> not_between;
  {
    const int p_tilde = ny + nz;
    if (nz > 0) {
      std::vector<bool> is_bz(p_tilde, false);
      for (int idx : lp.between_idx) is_bz[idx-1] = true;
      not_between.reserve(ny);
      for (int i = 0; i < p_tilde; ++i) if (!is_bz[i]) not_between.push_back(i);
    } else {
      not_between.resize(ny);
      std::iota(not_between.begin(), not_between.end(), 0);
    }
  }
  const int p_tilde = (int)(not_between.size() + nz);

  // Lookup: tilde index → compressed ny/nz index (-1 if absent)
  std::vector<int> tilde_to_nb(p_tilde, -1), tilde_to_bz(p_tilde, -1);
  for (int i = 0; i < ny; ++i) tilde_to_nb[not_between[i]] = i;
  for (int j = 0; j < nz; ++j) tilde_to_bz[lp.between_idx[j]-1] = j;

  // S_PW subset to not_between rows/cols
  MatrixXd S_PW = gd.Sigma_W;
  if (nz > 0) {
    MatrixXd tmp(ny, ny);
    for (int j = 0; j < ny; ++j)
      for (int i = 0; i < ny; ++i)
        tmp(i, j) = gd.Sigma_W(not_between[i], not_between[j]);
    S_PW = tmp;
  }

  // Invert sigma_w
  Eigen::LLT<MatrixXd> llt_w(imp.sigma_w);
  if (llt_w.info() != Eigen::Success) return false;
  const MatrixXd sigma_w_inv = llt_w.solve(MatrixXd::Identity(ny, ny));
  if (!sigma_w_inv.allFinite()) return false;

  // B = sigma_yz * sigma_zz_inv, sigma_b_z = sigma_b - sigma_yz * B^T
  MatrixXd sigma_zz_inv, B;
  MatrixXd sigma_b_z = imp.sigma_b;
  if (nz > 0) {
    Eigen::LLT<MatrixXd> llt_zz(imp.sigma_zz);
    if (llt_zz.info() != Eigen::Success) return false;
    sigma_zz_inv = llt_zz.solve(MatrixXd::Identity(nz, nz));
    if (!sigma_zz_inv.allFinite()) return false;
    B         = imp.sigma_yz * sigma_zz_inv;
    sigma_b_z = imp.sigma_b - imp.sigma_yz * B.transpose();
  }

  // N_m1 = sum(cluster_size - 1)
  double N_m1 = 0.0;
  for (int c : lp.cluster_size) N_m1 += (c - 1.0);

  // Gradient accumulators in compressed (ny/nz) space
  MatrixXd D_sigma_w   = 0.5 * N_m1 * (sigma_w_inv * S_PW * sigma_w_inv - sigma_w_inv);
  MatrixXd D_sigma_b_z = MatrixXd::Zero(ny, ny);
  MatrixXd D_B         = MatrixXd::Zero(ny, nz);   // d(ll)/d(B)
  MatrixXd D_sigma_zz_direct = MatrixXd::Zero(nz, nz);
  VectorXd D_mu_y      = VectorXd::Zero(ny);
  VectorXd D_mu_z      = VectorXd::Zero(nz);

  // mu_concat: [mu_z (nz), mu_y (ny)]
  VectorXd mu_concat(nz + ny);
  for (int i = 0; i < nz; ++i) mu_concat(i)    = imp.mu_z(i);
  for (int i = 0; i < ny; ++i) mu_concat(nz+i) = imp.mu_y(i);

  for (int clz = 0; clz < (int)gd.cov_d.size(); ++clz) {
    const double nj   = (double)lp.cluster_sizes[clz];
    const double ns_j = (double)lp.cluster_size_ns[clz];
    const VectorXd diff = gd.mean_d[clz] - mu_concat;

    const MatrixXd sigma_j = nj * sigma_b_z + imp.sigma_w;
    Eigen::LLT<MatrixXd> llt_j(sigma_j);
    if (llt_j.info() != Eigen::Success) return false;
    const MatrixXd sigma_j_inv = llt_j.solve(MatrixXd::Identity(ny, ny));
    if (!sigma_j_inv.allFinite()) return false;

    if (nz > 0) {
      const VectorXd diff_z = diff.head(nz);
      const VectorXd diff_y = diff.tail(ny);
      const MatrixXd Y2Yc      = gd.cov_d[clz] + diff * diff.transpose();
      const MatrixXd Y2Yc_zz   = Y2Yc.topLeftCorner(nz, nz);
      const MatrixXd Y2Yc_yz_m = Y2Yc.bottomLeftCorner(ny, nz);
      const MatrixXd Y2Yc_yy   = Y2Yc.bottomRightCorner(ny, ny);

      const MatrixXd BtSji  = B.transpose() * sigma_j_inv;     // nz x ny
      const MatrixXd Vinv11 = sigma_zz_inv + nj * (BtSji * B); // nz x nz

      // Effective residual for sigma_j gradient
      const MatrixXd M_j = Y2Yc_yy - 2.0 * Y2Yc_yz_m * B.transpose()
                           + B * Y2Yc_zz * B.transpose();
      const MatrixXd G_j = 0.5 * ns_j * (nj * sigma_j_inv * M_j * sigma_j_inv - sigma_j_inv);

      D_sigma_w   += G_j;
      D_sigma_b_z += nj * G_j;
      D_B         += ns_j * nj * sigma_j_inv * (Y2Yc_yz_m - B * Y2Yc_zz);
      // Direct sigma_zz contributions: log|sigma_zz| and tr(sigma_zz_inv * Y2Yc_zz)
      D_sigma_zz_direct -= 0.5 * ns_j * sigma_zz_inv;
      D_sigma_zz_direct += 0.5 * ns_j * sigma_zz_inv * Y2Yc_zz * sigma_zz_inv;

      D_mu_y += ns_j * nj * sigma_j_inv * (diff_y - B * diff_z);
      D_mu_z += ns_j * (Vinv11 * diff_z - nj * BtSji * diff_y);
    } else {
      const MatrixXd Y2Yc = gd.cov_d[clz] + diff * diff.transpose();
      const MatrixXd G_j  = 0.5 * ns_j * (nj * sigma_j_inv * Y2Yc * sigma_j_inv - sigma_j_inv);
      D_sigma_w   += G_j;
      D_sigma_b_z += nj * G_j;
      D_mu_y      += ns_j * nj * sigma_j_inv * diff;
    }
  }

  // Chain rule: sigma_b_z, B → sigma_b, sigma_yz, sigma_zz
  MatrixXd D_sigma_yz_final, D_sigma_zz_final;
  if (nz > 0) {
    D_sigma_yz_final = D_B * sigma_zz_inv - 2.0 * D_sigma_b_z * B;
    D_sigma_zz_final = B.transpose() * D_sigma_b_z * B
                     + B.transpose() * D_B * sigma_zz_inv
                     + D_sigma_zz_direct;
  }

  // Map from compressed space to block (within)
  const int pw = (int)lp.ov_idx_w.size();
  MatrixXd D_SigmaW(pw, pw);
  MatrixXd D_MuW = MatrixXd::Zero(pw, 1);
  for (int k = 0; k < pw; ++k) {
    const int nb_k = tilde_to_nb[lp.ov_idx_w[k]-1];
    D_MuW(k, 0) = (nb_k >= 0) ? D_mu_y(nb_k) : 0.0;
    for (int l = 0; l < pw; ++l) {
      const int nb_l = tilde_to_nb[lp.ov_idx_w[l]-1];
      D_SigmaW(k, l) = (nb_k>=0 && nb_l>=0) ? D_sigma_w(nb_k, nb_l) : 0.0;
    }
  }

  // Map from compressed space to block (between)
  const int pb = (int)lp.ov_idx_b.size();
  MatrixXd D_SigmaB(pb, pb);
  MatrixXd D_MuB = MatrixXd::Zero(pb, 1);
  for (int k = 0; k < pb; ++k) {
    const int nb_k = tilde_to_nb[lp.ov_idx_b[k]-1];
    const int bz_k = tilde_to_bz[lp.ov_idx_b[k]-1];
    if      (nb_k >= 0) D_MuB(k, 0) = D_mu_y(nb_k);
    else if (bz_k >= 0) D_MuB(k, 0) = D_mu_z(bz_k);
    for (int l = 0; l < pb; ++l) {
      const int nb_l = tilde_to_nb[lp.ov_idx_b[l]-1];
      const int bz_l = tilde_to_bz[lp.ov_idx_b[l]-1];
      double val = 0.0;
      if      (nb_k>=0 && nb_l>=0) val = D_sigma_b_z(nb_k, nb_l);
      else if (nz > 0 && nb_k>=0 && bz_l>=0) val = D_sigma_yz_final(nb_k, bz_l);
      else if (nz > 0 && bz_k>=0 && nb_l>=0) val = D_sigma_yz_final(nb_l, bz_k);
      else if (nz > 0 && bz_k>=0 && bz_l>=0) val = D_sigma_zz_final(bz_k, bz_l);
      D_SigmaB(k, l) = val;
    }
  }

  block_lisrel_backprop(gd.within,  D_SigmaW,
                        gd.within.has_mean  ? D_MuW : MatrixXd(),
                        x_free, npar, grad_ll);
  block_lisrel_backprop(gd.between, D_SigmaB,
                        gd.between.has_mean ? D_MuB : MatrixXd(),
                        x_free, npar, grad_ll);
  return true;
}

inline TwoLevelGroupData_ extract_twolevel_group(const List& grp) {
  TwoLevelGroupData_ gd;
  const List within_r  = grp["within"];
  const List between_r = grp["between"];
  const List cs        = grp["cluster_stats"];
  const List lp_r      = cs["lp"];

  gd.within  = extract_block_spec(within_r);
  gd.between = extract_block_spec(between_r);

  gd.Sigma_W = as<MatrixXd>(cs["Sigma_W"]);

  const List cov_d_r = cs["cov_d"];
  gd.cov_d.resize(cov_d_r.size());
  for (int i = 0; i < (int)cov_d_r.size(); ++i)
    gd.cov_d[i] = as<MatrixXd>(cov_d_r[i]);

  const List mean_d_r = cs["mean_d"];
  gd.mean_d.resize(mean_d_r.size());
  for (int i = 0; i < (int)mean_d_r.size(); ++i) {
    NumericVector v = mean_d_r[i];
    gd.mean_d[i].resize(v.size());
    for (int j = 0; j < v.size(); ++j) gd.mean_d[i](j) = v[j];
  }
  NumericVector lx = cs["loglik_x"];
  gd.loglik_x = lx[0];

  IntegerVector both    = lp_r["both_idx"];
  IntegerVector within  = lp_r["within_idx"];
  IntegerVector between = lp_r["between_idx"];
  gd.lp.both_idx.assign(both.begin(),    both.end());
  gd.lp.within_idx.assign(within.begin(),  within.end());
  gd.lp.between_idx.assign(between.begin(), between.end());

  const List ov_idx_r = lp_r["ov_idx"];
  IntegerVector ov_w = ov_idx_r[0];
  IntegerVector ov_b = ov_idx_r[1];
  gd.lp.ov_idx_w.assign(ov_w.begin(), ov_w.end());
  gd.lp.ov_idx_b.assign(ov_b.begin(), ov_b.end());

  IntegerVector nobs_v      = lp_r["nobs"];
  IntegerVector nclusters_v = lp_r["nclusters"];
  gd.lp.nobs      = nobs_v[0];
  gd.lp.nclusters = nclusters_v[0];

  IntegerVector cs_v   = lp_r["cluster_size"];
  IntegerVector css_v  = lp_r["cluster_sizes"];
  IntegerVector csns_v = lp_r["cluster_size_ns"];
  gd.lp.cluster_size.assign(cs_v.begin(),    cs_v.end());
  gd.lp.cluster_sizes.assign(css_v.begin(),  css_v.end());
  gd.lp.cluster_size_ns.assign(csns_v.begin(), csns_v.end());

  LogicalVector hx = lp_r["has_x"];
  gd.lp.has_x = (bool)hx[0];
  return gd;
}

inline std::vector<TwoLevelGroupData_> extract_twolevel_groups(const List& model) {
  const List groups = model["groups"];
  std::vector<TwoLevelGroupData_> gds(groups.size());
  for (int g = 0; g < (int)groups.size(); ++g)
    gds[g] = extract_twolevel_group(groups[g]);
  return gds;
}

// Definition of the forward-declared helper
bool twolevel_loglik_and_grad_impl(const List& model,
                                    const std::vector<double>& xf, int np,
                                    double& total_ll, std::vector<double>& gv) {
  const std::vector<TwoLevelGroupData_> tgds = extract_twolevel_groups(model);
  for (const auto& gd : tgds) {
    total_ll += twolevel_loglik_cpp(gd, xf);
    if (!twolevel_loglik_grad_cpp(gd, xf, np, gv)) return false;
  }
  return true;
}

// ---------------------------------------------------------------------------
// Batch marginals scan: evaluates loglik at all (param j, grid point k) pairs
// and computes gamma1 corrections using the central-difference shortcut.
//
// Parameters that require the rho*sd1*sd2 covariance correction (i.e. when
// sd1sd2 != 1) are NOT supported — call from R only when all sd1sd2 == 1.
// Transforms: 0 = identity, 1 = log/exp (variance), 2 = atanh/tanh (cor/cov)
//
// Rectangular Vscan / L_chol support (ceq.simple models):
//   theta_star: n_full-vector (full parameter space)
//   Vscan:      n_full x n_marginals (can be non-square; one column per marginal)
//   L_chol:     n_full x n_lchol_cols (= ceq.K %*% L_r for ceq.simple)
//   transforms, cov_var_idx1/2: n_full-vectors
//
// Returns a List with:
//   "loglik_scan" : n_marginals x K NumericMatrix
//   "gamma1"      : n_marginals-vector of gamma1j corrections
// [[Rcpp::export]]
List cpp_marginals_batch(
    List            model,
    NumericVector   theta_star,   // n-vector: MAP in theta-space
    NumericMatrix   Vscan,        // n x n_marginals: column j = scan direction for marginal j
    NumericMatrix   L_chol,       // n x n_lchol_cols: lower-Chol columns (may be non-square)
    IntegerVector   transforms,   // n-vector: 0/1/2
    NumericVector   z_grid,       // K-vector: grid for loglik scan
    IntegerVector   cov_var_idx1, // n-vector: 1-based var param index for cov type-2; 0 if none
    IntegerVector   cov_var_idx2, // n-vector: same for second variance
    NumericMatrix   H_prior_neg,  // n x n: negative prior Hessian at theta_star; zeros = no prior
    double delta_outer = 0.01,    // outer FD step (rate of change of Hessian)
    double h_inner     = 1e-5,    // inner central-diff step for Hessian trace
    int    nthreads    = 1        // number of OpenMP threads
) {
  const int n            = theta_star.size();
  const int n_marginals  = Vscan.ncol();       // number of marginals to compute
  const int n_lchol_cols = L_chol.ncol();      // columns of L used for trace computation
  const int K            = z_grid.size();
  const std::vector<GcpBlockCpp> gcp_blocks = extract_gcp_blocks_cpp(model);
  const PriorMetaCpp prior_meta = extract_prior_meta_cpp(model);

  // Pre-extract all R data into C++ structs (thread-safe; done before parallel)
  const std::string mb_type =
    as<std::string>(as<CharacterVector>(model["type"])[0]);
  const bool mb_twolevel = (mb_type == "lisrel_ml_twolevel");

  std::vector<GroupData_>          gds;
  std::vector<TwoLevelGroupData_>  tgds;
  if (mb_twolevel) {
    tgds = extract_twolevel_groups(model);
  } else {
    gds  = extract_groups(model);
  }
  const int ng = mb_twolevel ? (int)tgds.size() : (int)gds.size();

  // Copy R vectors into std::vectors (thread-safe)
  std::vector<double> ts_v(n), z_v(K);
  std::vector<int>    tr_v(n), cvi1(n), cvi2(n);
  std::vector<double> Vscan_v(n * n_marginals);
  std::vector<double> Lchol_v(n * n_lchol_cols);
  std::vector<double> Hpr_v(n * n, 0.0); // negative prior Hessian (column-major)
  for (int i = 0; i < n; ++i) {
    ts_v[i] = theta_star[i];
    tr_v[i] = transforms[i];
    cvi1[i] = cov_var_idx1[i];
    cvi2[i] = cov_var_idx2[i];
  }
  for (int k = 0; k < K; ++k) z_v[k] = z_grid[k];
  for (int j = 0; j < n_marginals; ++j)
    for (int i = 0; i < n; ++i)
      Vscan_v[i + j*n] = Vscan(i, j);
  for (int j = 0; j < n_lchol_cols; ++j)
    for (int i = 0; i < n; ++i)
      Lchol_v[i + j*n] = L_chol(i, j);
  for (int j = 0; j < n; ++j)
    for (int i = 0; i < n; ++i)
      Hpr_v[i + j*n] = H_prior_neg(i, j);

  // Output (use std::vector for thread-safe element writes)
  std::vector<double> loglik_flat(n_marginals * K, -1e40);
  std::vector<double> gamma1_v(n_marginals, 0.0);

  // Thread-local helpers: theta → x_free (lavaan-scale covariances for type-2)
  auto apply_tr = [&](const std::vector<double>& theta, std::vector<double>& xf) -> bool {
    return theta_to_x_gcp_cpp(theta, tr_v, cvi1, cvi2, gcp_blocks, xf, nullptr);
  };

  // neg_ll: returns -loglik (no prior), thread-safe
  auto neg_ll = [&](const std::vector<double>& theta) -> double {
    std::vector<double> xf(n);
    if (!apply_tr(theta, xf)) return 1e40;
    double total = 0.0;
    if (mb_twolevel) {
      for (int g = 0; g < ng; ++g)
        total -= twolevel_loglik_cpp(tgds[g], xf);
    } else {
      for (int g = 0; g < ng; ++g)
        total -= group_loglik_from_data(gds[g], xf);
    }
    if (!std::isfinite(total)) return 1e40;
    return total;
  };

  // neg_fg2: -loglik value + gradient in theta-space (including prior approx)
  auto neg_fg2 = [&](const std::vector<double>& theta,
                     double& fval, std::vector<double>& gval) -> bool {
    if (mb_twolevel) {
      std::vector<double> xf(n);
      apply_tr(theta, xf);
      double ll_total = 0.0;
      std::vector<double> gx_ll(n, 0.0);
      for (int g = 0; g < ng; ++g) {
        ll_total += twolevel_loglik_cpp(tgds[g], xf);
        if (!twolevel_loglik_grad_cpp(tgds[g], xf, n, gx_ll)) return false;
      }
      fval = -ll_total;
      if (!R_finite(fval)) return false;
      gval.assign(n, 0.0);
      // Chain rule: d(neg_ll)/d(theta[i]) = -d(ll)/d(xf[i]) * d(xf[i])/d(theta[i])
      for (int i = 0; i < n; ++i) {
        double chain;
        switch (tr_v[i]) {
          case 1:  chain = xf[i]; break;
          case 2:
            if (cvi1[i] > 0) {
              const double rho = std::tanh(theta[i]);
              const double sd1 = std::sqrt(std::exp(theta[cvi1[i]-1]));
              const double sd2 = std::sqrt(std::exp(theta[cvi2[i]-1]));
              chain = (1.0 - rho * rho) * sd1 * sd2;
            } else {
              chain = 1.0 - xf[i] * xf[i];
            }
            break;
          case 3:
            chain = 0.0;
            break;
          default: chain = 1.0;
        }
        gval[i] = -gx_ll[i] * chain;
      }
      for (int i = 0; i < n; ++i) {
        if (tr_v[i] == 2 && cvi1[i] > 0) {
          const double half_cov = 0.5 * xf[i];
          gval[cvi1[i] - 1] += -gx_ll[i] * half_cov;
          gval[cvi2[i] - 1] += -gx_ll[i] * half_cov;
        }
      }
      if (!gcp_blocks.empty()) {
        std::vector<GcpBlockEvalCpp> gcp_eval;
        std::vector<double> xf_gcp;
        if (!theta_to_x_gcp_cpp(theta, tr_v, cvi1, cvi2, gcp_blocks, xf_gcp, &gcp_eval)) return false;
        for (int b = 0; b < (int)gcp_blocks.size(); ++b) {
          const auto& blk = gcp_blocks[b];
          VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
            const int idx = blk.theta_idx0[k];
            if (cvi1[idx] > 0) {
              const double sd1 = std::sqrt(std::exp(theta[cvi1[idx] - 1]));
              const double sd2 = std::sqrt(std::exp(theta[cvi2[idx] - 1]));
              rho_grad[k] = -gx_ll[idx] * sd1 * sd2;
            } else {
              rho_grad[k] = -gx_ll[idx];
            }
          }
          const VectorXd theta_grad = gcp_eval[b].J.transpose() * rho_grad;
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) gval[blk.theta_idx0[k]] += theta_grad[k];
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
            const int idx = blk.theta_idx0[k];
            if (cvi1[idx] > 0) {
              const double half_cov = 0.5 * xf_gcp[idx];
              gval[cvi1[idx] - 1] += -gx_ll[idx] * half_cov;
              gval[cvi2[idx] - 1] += -gx_ll[idx] * half_cov;
            }
          }
        }
      }
    } else {
      std::vector<double> xf(n);
      std::vector<GcpBlockEvalCpp> gcp_eval;
      if (!theta_to_x_gcp_cpp(theta, tr_v, cvi1, cvi2, gcp_blocks, xf, &gcp_eval)) return false;
      fval = 0.0;
      std::vector<double> gx(n, 0.0);
      for (int g = 0; g < ng; ++g) {
        if (!group_loglik_grad_from_data(gds[g], xf, n, fval, gx)) return false;
      }
      if (!R_finite(fval)) return false;
      gval.assign(n, 0.0);
      // Diagonal chain rule: d(loglik)/d(theta[i])
      for (int i = 0; i < n; ++i) {
        double chain;
        switch (tr_v[i]) {
          case 1:  chain = xf[i]; break;
          case 2:
            if (cvi1[i] > 0) {
              const double rho  = std::tanh(theta[i]);
              const double sd1  = std::sqrt(std::exp(theta[cvi1[i]-1]));
              const double sd2  = std::sqrt(std::exp(theta[cvi2[i]-1]));
              chain = (1.0 - rho * rho) * sd1 * sd2;
            } else {
              chain = 1.0 - xf[i] * xf[i];
            }
            break;
          case 3:
            chain = 0.0;
            break;
          default: chain = 1.0;
        }
        gval[i] += gx[i] * chain;
      }
      // Off-diagonal jcb_mat corrections
      for (int i = 0; i < n; ++i) {
        if (tr_v[i] == 2 && cvi1[i] > 0) {
          const double half_cov = 0.5 * xf[i];
          gval[cvi1[i] - 1] += gx[i] * half_cov;
          gval[cvi2[i] - 1] += gx[i] * half_cov;
        }
      }
      for (int b = 0; b < (int)gcp_blocks.size(); ++b) {
        const auto& blk = gcp_blocks[b];
        VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
          const int idx = blk.theta_idx0[k];
          if (cvi1[idx] > 0) {
            const double sd1 = std::sqrt(std::exp(theta[cvi1[idx] - 1]));
            const double sd2 = std::sqrt(std::exp(theta[cvi2[idx] - 1]));
            rho_grad[k] = gx[idx] * sd1 * sd2;
          } else {
            rho_grad[k] = gx[idx];
          }
        }
        const VectorXd theta_grad = gcp_eval[b].J.transpose() * rho_grad;
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) gval[blk.theta_idx0[k]] += theta_grad[k];
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
          const int idx = blk.theta_idx0[k];
          if (cvi1[idx] > 0) {
            const double half_cov = 0.5 * xf[idx];
            gval[cvi1[idx] - 1] += gx[idx] * half_cov;
            gval[cvi2[idx] - 1] += gx[idx] * half_cov;
          }
        }
      }
    }  // end else (single-level analytical path)

    // Prior gradient (quadratic approx, both paths): H_prior @ (theta - theta_star)
    for (int i = 0; i < n; ++i) {
      double prior_gi = 0.0;
      for (int kk = 0; kk < n; ++kk)
        prior_gi += Hpr_v[i + kk*n] * (theta[kk] - ts_v[kk]);
      gval[i] += prior_gi;
    }
    return true;
  };

#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#endif

  // ===========================================================================
  // Main loop: parallelised over marginals j (0..n_marginals-1)
  // ===========================================================================
#ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
  for (int j = 0; j < n_marginals; ++j) {
    // vj = Vscan[:,j]
    std::vector<double> vj(n);
    for (int i = 0; i < n; ++i) vj[i] = Vscan_v[i + j*n];

    // th_plus = ts + vj * delta_outer
    std::vector<double> th_plus(n);
    for (int i = 0; i < n; ++i) th_plus[i] = ts_v[i] + vj[i] * delta_outer;

    // ---- 1. gamma1j (shortcut central-diff) ---------------------------------
    double trace_Hz1 = 0.0;
    for (int kk = 0; kk < n_lchol_cols; ++kk) {
      std::vector<double> Lk(n);
      for (int i = 0; i < n; ++i) Lk[i] = Lchol_v[i + kk*n];

      std::vector<double> th_fwd(n), th_bwd(n);
      for (int i = 0; i < n; ++i) {
        th_fwd[i] = th_plus[i] + Lk[i] * h_inner;
        th_bwd[i] = th_plus[i] - Lk[i] * h_inner;
      }
      double f_fwd, f_bwd;
      std::vector<double> g_fwd, g_bwd;
      if (!neg_fg2(th_fwd, f_fwd, g_fwd)) continue;
      if (!neg_fg2(th_bwd, f_bwd, g_bwd)) continue;
      for (int i = 0; i < n; ++i)
        trace_Hz1 += Lk[i] * (g_fwd[i] - g_bwd[i]);
    }
    trace_Hz1 /= (2.0 * h_inner);

    // vj^T H(th_plus) vj
    std::vector<double> th_fv(n), th_bv(n);
    for (int i = 0; i < n; ++i) {
      th_fv[i] = th_plus[i] + vj[i] * h_inner;
      th_bv[i] = th_plus[i] - vj[i] * h_inner;
    }
    double f_fv, f_bv;
    std::vector<double> gfv, gbv;
    double vHv_1 = 0.0;
    if (neg_fg2(th_fv, f_fv, gfv) && neg_fg2(th_bv, f_bv, gbv)) {
      for (int i = 0; i < n; ++i)
        vHv_1 += vj[i] * (gfv[i] - gbv[i]);
      vHv_1 /= (2.0 * h_inner);
    }

    // n_lchol_cols replaces n here: the trace bias correction uses the number
    // of effective free parameters (= n_marginals = ncol(L_chol))
    gamma1_v[j] = -0.5 * (trace_Hz1 - n_lchol_cols) / delta_outer + 0.5 * (vHv_1 - 1.0) / delta_outer;

    // ---- 2. Loglik scan ------------------------------------------------------
    for (int k = 0; k < K; ++k) {
      std::vector<double> th_k(n);
      for (int i = 0; i < n; ++i) th_k[i] = ts_v[i] + vj[i] * z_v[k];
      double lp_k = -neg_ll(th_k);   // +loglik
      if (!std::isfinite(lp_k)) {
        loglik_flat[j + k*n_marginals] = -1e40;
        continue;
      }
      if (!gcp_blocks.empty() && !prior_meta.free_id0.empty()) {
        double prior_lp = 0.0;
        if (!prior_lp_grad_cpp(th_k, prior_meta, gcp_blocks, prior_lp, nullptr, nullptr) ||
            !std::isfinite(prior_lp)) {
          lp_k = -1e40;
        } else {
          lp_k += prior_lp;
        }
      }
      loglik_flat[j + k*n_marginals] = lp_k;
    }
  }

  // Pack outputs back into R objects
  NumericMatrix loglik_scan(n_marginals, K);
  NumericVector gamma1(n_marginals);
  for (int j = 0; j < n_marginals; ++j) {
    gamma1[j] = gamma1_v[j];
    for (int k = 0; k < K; ++k)
      loglik_scan(j, k) = loglik_flat[j + k*n_marginals];
  }

  return List::create(
    Named("loglik_scan") = loglik_scan,
    Named("gamma1")      = gamma1
  );
}

// ---------------------------------------------------------------------------
// Full-native L-BFGS optimizer for LISREL ML log-likelihood.
// Transforms: 0 = identity, 1 = log/exp (variance), 2 = atanh/tanh (cor/cov)
// No R calls during the optimization loop.
// [[Rcpp::export]]
List cpp_lisrel_optimize(
    List model,
    NumericVector par0,
    IntegerVector transforms,
    int max_iter  = 200,
    double grad_tol = 1e-8,
    int m_bfgs   = 10,
    int max_ls    = 40,
    double h = 1e-5
) {
  const int n  = par0.size();
  List groups  = model["groups"];
  const int ng = groups.size();
  const std::vector<GcpBlockCpp> gcp_blocks = extract_gcp_blocks_cpp(model);
  const PriorMetaCpp prior_meta = extract_prior_meta_cpp(model);
  std::vector<int> cov_var_idx1, cov_var_idx2;
  extract_cov_idx_cpp(model, n, cov_var_idx1, cov_var_idx2);
  const std::string opt_type =
    as<std::string>(as<CharacterVector>(model["type"])[0]);
  const bool opt_twolevel = (opt_type == "lisrel_ml_twolevel");

  // ---------------------------------------------------------------------------
  // Compute -loglik and -grad in pars-space (applies ginv transforms + chain rule)
  // Returns false on failure.
  auto fg = [&](const VectorXd& pars, double& fval, VectorXd& gval) -> bool {
    // Convert pars → x_free
    std::vector<double> theta_v(n);
    std::vector<int> tr_v(n);
    for (int i = 0; i < n; ++i) {
      theta_v[i] = pars[i];
      tr_v[i] = transforms[i];
    }
    std::vector<GcpBlockEvalCpp> gcp_eval;
    std::vector<double> x_vec;
    if (!theta_to_x_gcp_cpp(theta_v, tr_v, cov_var_idx1, cov_var_idx2, gcp_blocks, x_vec, &gcp_eval)) {
      return false;
    }
    NumericVector x_free(n);
    for (int i = 0; i < n; ++i) x_free[i] = x_vec[i];

    NumericVector grad_x(n, 0.0);
    if (opt_twolevel) {
      // Twolevel: use cpp_lisrel_loglik_and_grad (FD gradient in x-space)
      List res = cpp_lisrel_loglik_and_grad(model, x_free);
      double ll = as<double>(res["loglik"]);
      if (!R_finite(ll)) return false;
      fval = -ll;
      NumericVector gx = res["grad"];
      for (int i = 0; i < n; ++i) grad_x[i] = -gx[i];
    } else {
      fval = 0.0;
      for (int g = 0; g < ng; ++g) {
        double ll_g = 0.0;
        NumericVector gg(n, 0.0);
        if (!lisrel_loglik_and_grad_group_impl(groups[g], x_free, n, ll_g, gg)) {
          return false;
        }
        fval -= ll_g;
        for (int i = 0; i < n; ++i) grad_x[i] -= gg[i];
      }
      if (!R_finite(fval)) return false;
    }

    // Chain rule: grad_pars = grad_x * ginv_prime(pars)
    gval.resize(n);
    gval.setZero();
    for (int i = 0; i < n; ++i) {
      double chain;
      switch (transforms[i]) {
        case 1:  chain = x_free[i]; break;                            // exp
        case 2:
          if (cov_var_idx1[i] > 0) {
            const double rho = std::tanh(pars[i]);
            const double sd1 = std::sqrt(std::exp(pars[cov_var_idx1[i] - 1]));
            const double sd2 = std::sqrt(std::exp(pars[cov_var_idx2[i] - 1]));
            chain = (1.0 - rho * rho) * sd1 * sd2;
          } else {
            chain = 1.0 - x_free[i] * x_free[i];
          }
          break;
        default: chain = 1.0;
      }
      gval[i] = grad_x[i] * chain;
    }
    for (int i = 0; i < n; ++i) {
      if (transforms[i] == 2 && cov_var_idx1[i] > 0) {
        const double half_cov = 0.5 * x_free[i];
        gval[cov_var_idx1[i] - 1] += grad_x[i] * half_cov;
        gval[cov_var_idx2[i] - 1] += grad_x[i] * half_cov;
      }
    }
    for (int b = 0; b < (int)gcp_blocks.size(); ++b) {
      const auto& blk = gcp_blocks[b];
      if (gcp_eval[b].J.size() == 0) return false;
      VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        const int idx = blk.theta_idx0[k];
        if (cov_var_idx1[idx] > 0) {
          const double sd1 = std::sqrt(std::exp(pars[cov_var_idx1[idx] - 1]));
          const double sd2 = std::sqrt(std::exp(pars[cov_var_idx2[idx] - 1]));
          rho_grad[k] = grad_x[idx] * sd1 * sd2;
        } else {
          rho_grad[k] = grad_x[idx];
        }
      }
      const VectorXd theta_grad = gcp_eval[b].J.transpose() * rho_grad;
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        gval[blk.theta_idx0[k]] = theta_grad[k];
      }
      for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
        const int idx = blk.theta_idx0[k];
        if (cov_var_idx1[idx] > 0) {
          const double half_cov = 0.5 * x_free[idx];
          gval[cov_var_idx1[idx] - 1] += grad_x[idx] * half_cov;
          gval[cov_var_idx2[idx] - 1] += grad_x[idx] * half_cov;
        }
      }
    }
    if (!prior_meta.free_id0.empty()) {
      std::vector<double> gpr;
      double lp = 0.0;
      if (!prior_lp_grad_cpp(theta_v, prior_meta, gcp_blocks, lp, &gpr, &gcp_eval)) return false;
      fval -= lp;
      for (int i = 0; i < n; ++i) gval[i] -= gpr[i];
    }
    return true;
  };

  // ---------------------------------------------------------------------------
  // Initialise
  VectorXd pars(n);
  for (int i = 0; i < n; ++i) pars[i] = par0[i];

  double f;
  VectorXd g(n);
  if (!fg(pars, f, g)) {
    return List::create(
      Named("par")       = par0,
      Named("loglik")    = R_NegInf,
      Named("hessian")   = NumericMatrix(n, n),
      Named("converged") = false,
      Named("n_iter")    = 0
    );
  }

  // ---------------------------------------------------------------------------
  // L-BFGS two-loop with backtracking Armijo line search
  std::vector<VectorXd> sv, yv;
  std::vector<double>   rhov;
  sv.reserve(m_bfgs); yv.reserve(m_bfgs); rhov.reserve(m_bfgs);

  int  n_iter   = 0;
  bool converged = false;

  while (n_iter < max_iter) {
    if (g.cwiseAbs().maxCoeff() < grad_tol) { converged = true; break; }

    // Two-loop L-BFGS search direction
    int k = static_cast<int>(sv.size());
    VectorXd q = g;
    std::vector<double> alps(k);
    for (int i = k - 1; i >= 0; --i) {
      alps[i] = rhov[i] * sv[i].dot(q);
      q -= alps[i] * yv[i];
    }
    VectorXd r = q;
    if (k > 0) {
      double gamma = sv[k-1].dot(yv[k-1]) / yv[k-1].squaredNorm();
      if (gamma > 0.0 && std::isfinite(gamma)) r *= gamma;
    }
    for (int i = 0; i < k; ++i) {
      double beta = rhov[i] * yv[i].dot(r);
      r += sv[i] * (alps[i] - beta);
    }
    VectorXd dir = -r;

    double slope = g.dot(dir);
    if (!std::isfinite(slope) || slope >= 0.0) {
      // Not a descent direction — reset and use steepest descent
      sv.clear(); yv.clear(); rhov.clear(); k = 0;
      dir = -g; slope = g.dot(dir);
    }

    // Backtracking Armijo line search
    double alpha_ls = 1.0;
    double f_new = 0.0;
    VectorXd g_new(n);
    bool ls_ok = false;
    for (int i = 0; i < max_ls; ++i) {
      VectorXd pars_new = pars + alpha_ls * dir;
      if (fg(pars_new, f_new, g_new) && f_new <= f + 1e-4 * alpha_ls * slope) {
        ls_ok = true;
        break;
      }
      alpha_ls *= 0.5;
      if (alpha_ls < 1e-16) break;
    }
    if (!ls_ok) break;

    VectorXd s = alpha_ls * dir;
    VectorXd y = g_new - g;
    double sy  = s.dot(y);

    // Update history only if curvature condition holds
    if (sy > 1e-10 * s.norm() * y.norm()) {
      if (k == m_bfgs) {
        sv.erase(sv.begin()); yv.erase(yv.begin()); rhov.erase(rhov.begin());
      }
      sv.push_back(s); yv.push_back(y); rhov.push_back(1.0 / sy);
    }

    pars += s;
    f     = f_new;
    g     = g_new;
    ++n_iter;
  }

  // ---------------------------------------------------------------------------
  // Forward-difference Hessian of -loglik at optimum.
  // Reuses the gradient `g` already computed at `pars` from the last L-BFGS
  // step — costs n gradient evaluations (one per column) instead of 2n.
  // The one-sided scheme introduces O(h) bias; symmetrising the result cancels
  // most of the anti-symmetric rounding error.
  MatrixXd H_neg(n, n);
  H_neg.setZero();
  // g is the gradient of -loglik at pars, already valid from the last iteration
  const VectorXd& g0 = g;
  for (int j = 0; j < n; ++j) {
    VectorXd pp = pars; pp[j] += h;
    double fp = 0.0;
    VectorXd gp(n);
    bool okp = fg(pp, fp, gp);
    for (int i = 0; i < n; ++i) {
      H_neg(i, j) = okp ? (gp[i] - g0[i]) / h : 0.0;
    }
  }
  H_neg = 0.5 * (H_neg + H_neg.transpose());

  NumericVector par_out(pars.data(), pars.data() + n);
  return List::create(
    Named("par")       = par_out,
    Named("loglik")    = -f,
    Named("grad")      = wrap(-g),   // analytic gradient of loglik at optimum
    Named("hessian")   = wrap(H_neg),
    Named("converged") = converged,
    Named("n_iter")    = n_iter
  );
}

// ---------------------------------------------------------------------------
// C++ NORTA correlation adjustment with OpenMP parallelism.
// For each pair (j,k) with non-trivial skewness, finds r_star such that the
// bivariate GH integral of the SN quantile functions equals the target
// Pearson correlation rho_jk. Uses linear interpolation on precomputed
// quantile grids and a hybrid secant/bisection root-finder.
// [[Rcpp::export]]
NumericMatrix cpp_norta_adjust(
    NumericVector z1,        // n GH nodes (= sqrt(2)*gauss_hermite$nodes)
    NumericVector ww_flat,   // n*n GH weights, column-major (outer(w, w))
    NumericMatrix q1_all,    // m x n quantile values at GH nodes
    NumericMatrix R_in,      // m x m target correlation matrix
    NumericVector mu_sn,     // m-vector: E[Q_j(U)] under N(0,1) U
    NumericVector sd_sn,     // m-vector: SD[Q_j(U)]
    NumericVector alphas,    // m-vector of skewness params
    NumericVector u_grid,    // K uniform grid in (eps, 1-eps)
    NumericMatrix q_grids,   // m x K quantile values at u_grid
    int nthreads = 1
) {
  const int m = R_in.nrow();
  const int n = z1.size();
  const int K = u_grid.size();
  const double eps = 1e-10;
  const double pi_inv = 1.0 / M_PI;

  // Flatten to thread-safe std::vectors (all read-only in the parallel section)
  std::vector<double> z1_v(z1.begin(), z1.end());
  std::vector<double> ww_v(ww_flat.begin(), ww_flat.end());
  std::vector<double> mu_v(mu_sn.begin(), mu_sn.end());
  std::vector<double> sd_v(sd_sn.begin(), sd_sn.end());
  std::vector<double> al_v(alphas.begin(), alphas.end());
  std::vector<double> ug_v(u_grid.begin(), u_grid.end());

  // q1_all: m × n → flat[j + k*m]
  std::vector<double> q1_v(m * n);
  for (int j = 0; j < m; ++j)
    for (int k = 0; k < n; ++k)
      q1_v[j + k*m] = q1_all(j, k);

  // q_grids: m × K → flat[j + k*m]
  std::vector<double> qg_v(m * K);
  for (int j = 0; j < m; ++j)
    for (int k = 0; k < K; ++k)
      qg_v[j + k*m] = q_grids(j, k);

  // Output: start from a copy of R_in
  std::vector<double> Rstar_v(m * m);
  for (int j = 0; j < m; ++j)
    for (int k = 0; k < m; ++k)
      Rstar_v[j + k*m] = R_in(j, k);

  // Collect non-trivial pairs to process
  std::vector<std::pair<int,int>> pairs;
  pairs.reserve(m * (m - 1) / 2);
  for (int j = 0; j < m - 1; ++j)
    for (int k = j + 1; k < m; ++k) {
      if (std::abs(Rstar_v[j + k*m]) < 1e-10) continue;
      if (std::abs(al_v[j]) < 0.01 && std::abs(al_v[k]) < 0.01) continue;
      pairs.push_back({j, k});
    }
  const int np = static_cast<int>(pairs.size());

#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
  for (int p = 0; p < np; ++p) {
    const int jj = pairs[p].first;
    const int kk = pairs[p].second;
    const double rho_target = Rstar_v[jj + kk*m];

    // Linear interpolation of quantile for marginal km at prob u
    auto qk = [&](double u) -> double {
      if (u <= ug_v[0])   return qg_v[kk + 0*m];
      if (u >= ug_v[K-1]) return qg_v[kk + (K-1)*m];
      int lo = 0, hi = K - 1;
      while (hi - lo > 1) {
        int mid = (lo + hi) >> 1;
        if (ug_v[mid] <= u) lo = mid; else hi = mid;
      }
      double t = (u - ug_v[lo]) / (ug_v[hi] - ug_v[lo]);
      return qg_v[kk + lo*m] + t * (qg_v[kk + hi*m] - qg_v[kk + lo*m]);
    };

    // GH integral: E[Q_j(Phi(Z1)) * Q_k(Phi(r*Z1 + sqrt(1-r^2)*Z2))]
    auto integrand_val = [&](double r_star) -> double {
      double s = std::sqrt(std::max(0.0, 1.0 - r_star * r_star));
      double cross = 0.0;
      for (int l = 0; l < n; ++l) {
        const double q1_jl = q1_v[jj + l*m];
        for (int i = 0; i < n; ++i) {
          double z2 = r_star * z1_v[i] + s * z1_v[l];
          double u2 = R::pnorm5(z2, 0.0, 1.0, 1, 0);
          u2 = std::max(eps, std::min(1.0 - eps, u2));
          cross += ww_v[i + l*n] * q1_jl * qk(u2);
        }
      }
      return cross * pi_inv;
    };

    // f(r) = corr(r) - rho_target; find root in [-0.999, 0.999]
    auto f = [&](double r) -> double {
      double cross = integrand_val(r);
      return (cross - mu_v[jj] * mu_v[kk]) / (sd_v[jj] * sd_v[kk]) - rho_target;
    };

    double a = -0.999, b = 0.999;
    double fa = f(a), fb = f(b);
    if (fa * fb > 0.0) continue;  // No root in interval — skip

    const double tol = 1e-6;
    for (int iter = 0; iter < 60; ++iter) {
      if (std::abs(b - a) < tol) break;
      // Secant step
      double s_br = b - fb * (b - a) / (fb - fa);
      // Fall back to bisection if secant lands outside (a, b)
      if (s_br <= std::min(a, b) || s_br >= std::max(a, b))
        s_br = 0.5 * (a + b);
      double fs = f(s_br);
      if (fa * fs < 0.0) { b = s_br; fb = fs; }
      else               { a = s_br; fa = fs; }
    }
    const double root = 0.5 * (a + b);
    Rstar_v[jj + kk*m] = root;
    Rstar_v[kk + jj*m] = root;
  }

  // Copy result to R matrix (NumericMatrix is column-major)
  NumericMatrix R_out(m, m);
  for (int j = 0; j < m; ++j)
    for (int k = 0; k < m; ++k)
      R_out(j, k) = Rstar_v[j + k*m];
  return R_out;
}

// ---------------------------------------------------------------------------
// C++ scalar implementation of INLA's GMRFLib_sn_Pinv (skew-normal quantile).
// Matches R's qsnorm_fast / .qsn_std_fast exactly.
// ---------------------------------------------------------------------------

// Lambert W_0 function via Halley's method (x > 0)
inline double plog_cpp(double x) {
  if (x <= 0.0) return 0.0;
  double w0 = std::log(1.2 * x / std::log(2.4 * x / std::log1p(2.4 * x)));
  for (int i = 0; i < 3; ++i) {
    double e   = std::exp(w0);
    double f   = w0 * e - x;
    double den = f * (2.0 + w0) - (e + e) * (1.0 + w0) * (1.0 + w0);
    w0 += ((f + f) * (1.0 + w0)) / den;
  }
  return w0;
}

// Standardised SN quantile: Q_{SN(0,1,alpha)}(u)  (scalar, matches .qsn_std_fast)
inline double qsn_std_fast_cpp(double u, double alpha) {
  const double SQRT2       = 1.4142135623730950488;
  const double CONST_TOL   = 1.6448536269514722;
  const double C_2PI       = 6.2831853071795864769;
  const double INV_PI      = 0.31830988618379067154;
  const double SN_SCALE    = 0.79788456080286535588; // sqrt(2/pi)
  const double TOL         = 0.01;

  if (u <= 0.0) return R_NegInf;
  if (u >= 1.0) return R_PosInf;

  bool flip = (alpha < 0.0);
  if (flip) { u = 1.0 - u; alpha = -alpha; }

  if (alpha == 0.0) {
    double r = R::qnorm5(u, 0.0, 1.0, 1, 0);
    return flip ? -r : r;
  }

  if (alpha == 1.0) u = std::sqrt(u);

  double z = R::qnorm5(u, 0.0, 1.0, 1, 0);
  double A = alpha;

  double right_limit = 2.0 * R::pnorm5((CONST_TOL / A) * SQRT2, 0.0, 1.0, 1, 0) - 1.0;

  double result;
  if (u > right_limit) {
    result = R::qnorm5((1.0 + u) / 2.0, 0.0, 1.0, 1, 0);
  } else {
    // errfn == 1 throughout .qsn_std_fast; simplify by dropping it
    double val_x = 0.5 - INV_PI * std::atan(A);
    double x_pt  = R::qnorm5(val_x, 0.0, 1.0, 1, 0);
    double expon = std::exp(-0.5 * x_pt * x_pt);
    double efder = expon * SN_SCALE * A;

    double c1 = expon;
    double c2 = -expon * (efder + x_pt) * 0.5;
    double c3 = (1.0/6.0) * expon *
                (3.0*efder*efder + (-1.0 + x_pt*x_pt) + expon*expon + 3.0*efder*x_pt);
    double c4 = -(1.0/24.0) * expon *
                (15.0*efder*efder*efder +
                 x_pt*(-3.0 + x_pt*x_pt) +
                 6.0*expon*expon*x_pt +
                 18.0*efder*efder*x_pt +
                 efder*((-4.0 + 7.0*x_pt*x_pt) + expon*expon*(7.0 - A*A)));
    double c5 = (1.0/120.0) * expon *
                (105.0*efder*efder*efder*efder +
                 (3.0 - 6.0*x_pt*x_pt + x_pt*x_pt*x_pt*x_pt) +
                 5.0*expon*expon*(-2.0 + 5.0*x_pt*x_pt) +
                 7.0*expon*expon*expon*expon +
                 150.0*efder*efder*efder*x_pt +
                 efder*(5.0*x_pt*(-5.0 + 3.0*x_pt*x_pt) +
                        10.0*expon*expon*x_pt*(7.0 - A*A)) +
                 5.0*efder*efder*(3.0*(-2.0 + 5.0*x_pt*x_pt) +
                                  expon*expon*(12.0 - 3.0*A*A)));

    double h = 0.75 * std::pow(std::abs(TOL / (std::abs(c5) + 1e-300)), 0.2);
    double left_limit = x_pt - h;

    if (z < left_limit) {
      double arg = 1.0 / (C_2PI * u * A);
      result = -std::sqrt(2.0 * plog_cpp(arg) / (1.0 + A*A));
    } else {
      double hv = z - x_pt;
      result = hv * (c1 + hv * (c2 + hv * (c3 + hv * (c4 + hv * c5))));
    }
  }

  return flip ? -result : result;
}

// Full skew-normal quantile: Q_{SN(xi,omega,alpha)}(u)
inline double qsnorm_fast_cpp(double u, double xi, double omega, double alpha) {
  return xi + omega * qsn_std_fast_cpp(u, alpha);
}

// ---------------------------------------------------------------------------
// Batch posterior sampling: Cholesky copula + quantile mapping + theta→x.
//
// All work is done in C++: quantile grids are computed here from the
// skew-normal parameters (xi, omega, alpha), eliminating the R precomputation
// loop that was the main bottleneck.
//
// Inputs:
//   z_raw      : nsamp x m standard normal draws
//   R_chol_ut  : m x m upper-triangular Cholesky of R_star (from chol(R))
//   sn_xi      : m-vector  (skew-normal location per parameter)
//   sn_omega   : m-vector  (skew-normal scale per parameter)
//   sn_alpha   : m-vector  (skew-normal shape per parameter)
//   transforms : m-vector (0=identity, 1=exp, 2=tanh or tanh*sd1*sd2)
//   cov_var_idx1, cov_var_idx2 : 1-based indices (0 means no scaling)
//   n_grid     : number of internal quantile grid points (default 2000)
//
// Returns: list(theta_samp = nsamp x m, x_samp = nsamp x m)
// [[Rcpp::export]]
List cpp_sample_batch(
    NumericMatrix z_raw,          // nsamp x m
    NumericMatrix R_chol_ut,      // m x m upper-triangular
    NumericVector sn_xi,          // m-vector
    NumericVector sn_omega,       // m-vector
    NumericVector sn_alpha,       // m-vector
    NumericVector transforms,     // m-vector
    IntegerVector cov_var_idx1,
    IntegerVector cov_var_idx2,
    int nthreads = 1,
    int n_grid   = 2000
) {
  const int nsamp = z_raw.nrow();
  const int m     = z_raw.ncol();
  const int K     = std::max(n_grid, 1000);
  const double eps = 1e-10;

  if (R_chol_ut.nrow() != m || R_chol_ut.ncol() != m)
    stop("Native posterior sampler: R_chol_ut must be m x m.");
  if (sn_xi.size() != m || sn_omega.size() != m || sn_alpha.size() != m)
    stop("Native posterior sampler: sn_xi/omega/alpha must have length m.");
  if (transforms.size() != m || cov_var_idx1.size() != m || cov_var_idx2.size() != m)
    stop("Native posterior sampler: transforms/cov_var_idx must have length m.");

  // Build uniform u-grid in (eps, 1-eps)
  std::vector<double> ug_v(K);
  for (int k = 0; k < K; ++k)
    ug_v[k] = eps + (1.0 - 2.0*eps) * k / (K - 1.0);

  // Precompute quantile grids in C++ (parallelisable, no R overhead)
  std::vector<double> qg_v(m * K);
#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#pragma omp parallel for schedule(static) num_threads(nthreads)
#endif
  for (int j = 0; j < m; ++j) {
    const double xi    = sn_xi[j];
    const double omega = sn_omega[j];
    const double alpha = sn_alpha[j];
    for (int k = 0; k < K; ++k)
      qg_v[j + k*m] = qsnorm_fast_cpp(ug_v[k], xi, omega, alpha);
  }

  // Flatten remaining inputs
  std::vector<double> z_v(nsamp * m);
  for (int s = 0; s < nsamp; ++s)
    for (int j = 0; j < m; ++j)
      z_v[s + j*nsamp] = z_raw(s, j);

  std::vector<double> L_v(m * m, 0.0);
  for (int j = 0; j < m; ++j)
    for (int k = 0; k < m; ++k)
      L_v[j + k*m] = R_chol_ut(j, k);

  std::vector<int> tr_v(transforms.begin(), transforms.end());
  std::vector<int> cvi1(cov_var_idx1.begin(), cov_var_idx1.end());
  std::vector<int> cvi2(cov_var_idx2.begin(), cov_var_idx2.end());

  // Linear interpolation on the precomputed quantile grid
  auto interp = [&](int j_marg, double u) -> double {
    if (u <= ug_v[0])   return qg_v[j_marg];
    if (u >= ug_v[K-1]) return qg_v[j_marg + (K-1)*m];
    int lo = 0, hi = K - 1;
    while (hi - lo > 1) { int mid = (lo + hi) >> 1; if (ug_v[mid] <= u) lo = mid; else hi = mid; }
    double t = (u - ug_v[lo]) / (ug_v[hi] - ug_v[lo]);
    return qg_v[j_marg + lo*m] + t * (qg_v[j_marg + hi*m] - qg_v[j_marg + lo*m]);
  };

  std::vector<double> theta_v(nsamp * m);
  std::vector<double> x_v(nsamp * m);

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(nthreads)
#endif
  for (int s = 0; s < nsamp; ++s) {
    // Step 1: z = z_raw[s,] %*% R_chol_ut  (row-vector times upper-triangular)
    std::vector<double> z(m, 0.0);
    for (int j = 0; j < m; ++j)
      for (int k = j; k < m; ++k)
        z[k] += z_v[s + j*nsamp] * L_v[j + k*m];

    // Step 2: u = pnorm(z); theta = quantile(u) via interpolation
    for (int j = 0; j < m; ++j) {
      double u = R::pnorm5(z[j], 0.0, 1.0, 1, 0);
      u = std::max(eps, std::min(1.0 - eps, u));
      theta_v[s + j*nsamp] = interp(j, u);
    }

    // Step 3: x = theta_to_x
    // First pass: compute x for variance params (needed for covariance scaling)
    std::vector<double> x_s(m);
    for (int j = 0; j < m; ++j) {
      double th = theta_v[s + j*nsamp];
      if (tr_v[j] == 1)      x_s[j] = std::exp(th);
      else if (tr_v[j] == 0) x_s[j] = th;
      else                   x_s[j] = std::tanh(th);  // placeholder for cov
    }
    // Second pass: covariance scaling
    for (int j = 0; j < m; ++j) {
      if (tr_v[j] == 2 && cvi1[j] > 0) {
        double rho = x_s[j];  // tanh(theta[j])
        double sd1 = std::sqrt(x_s[cvi1[j]-1]);
        double sd2 = std::sqrt(x_s[cvi2[j]-1]);
        x_s[j] = rho * sd1 * sd2;
      }
    }
    for (int j = 0; j < m; ++j) x_v[s + j*nsamp] = x_s[j];
  }

  NumericMatrix theta_out(nsamp, m), x_out(nsamp, m);
  for (int s = 0; s < nsamp; ++s)
    for (int j = 0; j < m; ++j) {
      theta_out(s, j) = theta_v[s + j*nsamp];
      x_out(s, j)     = x_v[s + j*nsamp];
    }
  return List::create(Named("theta") = theta_out, Named("x") = x_out);
}

// ---------------------------------------------------------------------------
// cpp_prior_logdens_scan
//
// Compute the prior log-density at all scan points (m params × K scan steps).
// Used to add the prior contribution to the loglik scan matrix from
// cpp_marginals_batch without the slow R double loop.
//
// Prior cache layout (matches prepare_priors_for_optim output):
//   free_id       : integer vector, 0-based theta indices that have a prior
//   trans_type    : 0=identity, 1=exp, 2=tanh  (transform theta → x for prior)
//   prior_type    : 1=normal, 2=gamma, 3=beta
//   p1, p2        : hyperparameters (mean/shape, sd/rate)
//   is_sd_prior   : Gamma prior is on SD, not variance
//   is_prec_prior : prior is on precision (1/x)
//
// Returns: m × K NumericMatrix.  result[j, k] = sum_i lp_i(theta_star + Vscan[,j]*z[k])
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
NumericMatrix cpp_prior_logdens_scan(
  NumericVector theta_star,      // m
  NumericMatrix Vscan,           // m × m (column j = scan direction for param j)
  NumericVector z_grid,          // K
  IntegerVector free_id,         // n_prior (0-based)
  IntegerVector trans_type,      // 0/1/2
  IntegerVector prior_type,      // 1/2/3
  NumericVector p1,
  NumericVector p2,
  LogicalVector is_sd_prior,
  LogicalVector is_prec_prior,
  int nthreads = 1
) {
  const int m       = theta_star.size();
  const int K       = z_grid.size();
  const int n_prior = free_id.size();

  // Copy to std::vector for thread-safe access
  std::vector<double> ts(m), z(K);
  for (int i = 0; i < m; ++i) ts[i] = theta_star[i];
  for (int k = 0; k < K; ++k) z[k] = z_grid[k];

  std::vector<int>    fid(n_prior), ttype(n_prior), ptype(n_prior);
  std::vector<double> vp1(n_prior), vp2(n_prior);
  std::vector<bool>   sd_pr(n_prior), prec_pr(n_prior);
  for (int i = 0; i < n_prior; ++i) {
    fid[i]    = free_id[i];
    ttype[i]  = trans_type[i];
    ptype[i]  = prior_type[i];
    vp1[i]    = p1[i];
    vp2[i]    = p2[i];
    sd_pr[i]  = (bool)is_sd_prior[i];
    prec_pr[i]= (bool)is_prec_prior[i];
  }

  // Column-major storage of Vscan: Vscan_v[i + j*m] = Vscan(i,j)
  std::vector<double> Vs(m * m);
  for (int j = 0; j < m; ++j)
    for (int i = 0; i < m; ++i)
      Vs[i + j*m] = Vscan(i, j);

  NumericMatrix result(m, K);
  std::vector<double> res_v(m * K, 0.0);

#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#endif

  // Helper: compute total prior log-density for a given theta vector
  // (only the prior-param indices matter)
  auto prior_lp = [&](const std::vector<double>& theta) -> double {
    double lp = 0.0;
    for (int i = 0; i < n_prior; ++i) {
      const double th_val = theta[fid[i]];
      double x_val, dx_dth;

      switch (ttype[i]) {
        case 1: {
          x_val  = std::exp(th_val);
          dx_dth = x_val;
          break;
        }
        case 2: {
          const double safe = 1.0 - 1e-6;
          const double tv   = std::tanh(th_val);
          x_val  = safe * tv;
          dx_dth = safe * (1.0 - tv * tv);
          break;
        }
        default: {
          x_val  = th_val;
          dx_dth = 1.0;
          break;
        }
      }

      if (prec_pr[i]) {
        const double prec = 1.0 / x_val;
        switch (ptype[i]) {
          case 1:
            lp += R::dnorm(prec, vp1[i], vp2[i], 1);
            break;
          case 2:
            lp += R::dgamma(prec, vp1[i], 1.0 / vp2[i], 1);
            break;
          default:
            break;
        }
        dx_dth /= (x_val * x_val);
      } else if (sd_pr[i]) {
        const double sd_val = std::sqrt(x_val);
        lp += R::dgamma(sd_val, vp1[i], 1.0 / vp2[i], 1);
        dx_dth /= (2.0 * sd_val);
      } else {
        switch (ptype[i]) {
          case 1:
            lp += R::dnorm(x_val, vp1[i], vp2[i], 1);
            break;
          case 2:
            if (x_val > 0.0)
              lp += R::dgamma(x_val, vp1[i], 1.0 / vp2[i], 1);
            else
              lp += -1e30;
            break;
          case 3: {
            const double y = (x_val + 1.0) / 2.0;
            lp += R::dbeta(y, vp1[i], vp2[i], 1) - 0.6931471805599453;
            break;
          }
          default: break;
        }
      }
      lp += std::log(std::abs(dx_dth));
    }
    return lp;
  };

#pragma omp parallel for collapse(2) schedule(static) if(nthreads > 1)
  for (int j = 0; j < m; ++j) {
    for (int k = 0; k < K; ++k) {
      // Build theta = theta_star + Vscan[,j] * z[k]
      // We only need theta[fid[i]] values
      std::vector<double> theta(m);
      const double zk = z[k];
      for (int ii = 0; ii < m; ++ii)
        theta[ii] = ts[ii] + Vs[ii + j*m] * zk;
      res_v[j + k*m] = prior_lp(theta);
    }
  }

  for (int j = 0; j < m; ++j)
    for (int k = 0; k < K; ++k)
      result(j, k) = res_v[j + k*m];

  return result;
}

// ---------------------------------------------------------------------------
// cpp_prior_logdens_vec
//
// Evaluate prior log-density at a batch of n_pts theta vectors (each m-dim).
// theta_mat: n_pts × m matrix (row-major from R perspective)
// Returns: n_pts-vector of prior log-density values.
// Same prior cache layout as cpp_prior_logdens_scan.
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
NumericVector cpp_prior_logdens_vec(
  NumericMatrix theta_mat,   // n_pts × m
  IntegerVector free_id,
  IntegerVector trans_type,
  IntegerVector prior_type,
  NumericVector p1,
  NumericVector p2,
  LogicalVector is_sd_prior,
  LogicalVector is_prec_prior,
  int nthreads = 1
) {
  const int n_pts   = theta_mat.nrow();
  const int m       = theta_mat.ncol();
  const int n_prior = free_id.size();

  std::vector<int>    fid(n_prior), ttype(n_prior), ptype(n_prior);
  std::vector<double> vp1(n_prior), vp2(n_prior);
  std::vector<bool>   sd_pr(n_prior), prec_pr(n_prior);
  for (int i = 0; i < n_prior; ++i) {
    fid[i]    = free_id[i];
    ttype[i]  = trans_type[i];
    ptype[i]  = prior_type[i];
    vp1[i]    = p1[i];
    vp2[i]    = p2[i];
    sd_pr[i]  = (bool)is_sd_prior[i];
    prec_pr[i]= (bool)is_prec_prior[i];
  }

  // Copy theta_mat to row-major std::vector for cache-friendly access
  std::vector<double> tm(n_pts * m);
  for (int s = 0; s < n_pts; ++s)
    for (int j = 0; j < m; ++j)
      tm[s * m + j] = theta_mat(s, j);

#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#endif

  std::vector<double> out(n_pts, 0.0);

#pragma omp parallel for schedule(static) if(nthreads > 1)
  for (int s = 0; s < n_pts; ++s) {
    double lp = 0.0;
    for (int i = 0; i < n_prior; ++i) {
      const double th_val = tm[s * m + fid[i]];
      double x_val, dx_dth;

      switch (ttype[i]) {
        case 1: {
          x_val  = std::exp(th_val);
          dx_dth = x_val;
          break;
        }
        case 2: {
          const double safe = 1.0 - 1e-6;
          const double tv   = std::tanh(th_val);
          x_val  = safe * tv;
          dx_dth = safe * (1.0 - tv * tv);
          break;
        }
        default: {
          x_val  = th_val;
          dx_dth = 1.0;
          break;
        }
      }

      if (prec_pr[i]) {
        const double prec = 1.0 / x_val;
        switch (ptype[i]) {
          case 1:  lp += R::dnorm(prec,  vp1[i], vp2[i],         1); break;
          case 2:  lp += R::dgamma(prec, vp1[i], 1.0/vp2[i], 1); break;
          default: break;
        }
        dx_dth /= (x_val * x_val);
      } else if (sd_pr[i]) {
        const double sd_val = std::sqrt(x_val);
        lp += R::dgamma(sd_val, vp1[i], 1.0 / vp2[i], 1);
        dx_dth /= (2.0 * sd_val);
      } else {
        switch (ptype[i]) {
          case 1: lp += R::dnorm(x_val, vp1[i], vp2[i], 1); break;
          case 2:
            if (x_val > 0.0) lp += R::dgamma(x_val, vp1[i], 1.0/vp2[i], 1);
            else              lp += -1e30;
            break;
          case 3: {
            const double y = (x_val + 1.0) / 2.0;
            lp += R::dbeta(y, vp1[i], vp2[i], 1) - 0.6931471805599453;
            break;
          }
          default: break;
        }
      }
      lp += std::log(std::abs(dx_dth));
    }
    out[s] = lp;
  }

  return wrap(out);
}

// ---------------------------------------------------------------------------
// cpp_vb_fg
//
// Compute the VB (variational Bayes) correction objective and gradient in one
// C++ batch call.  Replaces the R loop that called joint_lp / joint_lp_grad
// 101 times per nlminb iteration.
//
// Math:
//   mu_new = mu0 + L %*% delta
//   f = -mean_b joint_lp(mu_new + Z[b,])
//   g = t(L) %*% (-mean_b joint_lp_grad(mu_new + Z[b,]))
//
// ceq_K support (ceq.simple models):
//   When ceq_K has rows > 0, theta samples live in m_r-space (= delta.size())
//   but loglik is evaluated in m_full-space via: theta_full = ceq_K %*% theta_r.
//   transforms, cov_var_idx1/2 must be m_full-length in that case.
//   Gradient is contracted back: gl_r = t(ceq_K) %*% gl_full.
//
// Prior params use the same convention as cpp_prior_logdens_scan.
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
List cpp_vb_fg(
  NumericVector delta,          // m_r: current iterate in whitened space
  NumericVector mu0,            // m_r: VB mean (theta_star or shifted)
  NumericMatrix L,              // m_r×m_r: lower-Chol of Sigma_theta (column-major)
  NumericMatrix Z,              // (n_qmc+1) × m_r: QMC points (row 0 = zeros)
  List model,                   // native backend
  IntegerVector transforms,     // m (= m_full if has_ceq, else m_r): 0/1/2
  IntegerVector cov_var_idx1,   // m-vector: 1-based, 0 if none
  IntegerVector cov_var_idx2,   // m-vector: 1-based, 0 if none
  IntegerVector prior_free_id,  // n_prior: 0-based theta indices (in m_r space)
  IntegerVector prior_trans,    // 0/1/2
  IntegerVector prior_type,     // 1=normal 2=gamma 3=beta
  NumericVector prior_p1,
  NumericVector prior_p2,
  LogicalVector prior_is_sd,
  LogicalVector prior_is_prec,
  NumericMatrix ceq_K,          // m_full × m_r expansion matrix; 0 rows = no expansion
  int nthreads = 1
) {
  const int m_r     = delta.size();
  const bool has_ceq = (ceq_K.nrow() > 0);
  const int m_full  = has_ceq ? (int)ceq_K.nrow() : m_r;
  const int ns      = Z.nrow();
  const int n_prior = prior_free_id.size();

  // Copy inputs to std::vector for thread-safe access
  std::vector<double> del_v(m_r), mu0_v(m_r);
  std::vector<double> L_v(m_r * m_r);
  std::vector<int>    tr_v(m_full), cvi1(m_full), cvi2(m_full);
  for (int i = 0; i < m_r; ++i) {
    del_v[i]  = delta[i];
    mu0_v[i]  = mu0[i];
  }
  for (int i = 0; i < m_full; ++i) {
    tr_v[i]   = transforms[i];
    cvi1[i]   = cov_var_idx1[i];
    cvi2[i]   = cov_var_idx2[i];
  }
  for (int j = 0; j < m_r; ++j)
    for (int i = 0; i < m_r; ++i)
      L_v[i + j*m_r] = L(i, j);  // column-major

  // Compute mu_new = mu0 + L %*% delta  (lower-triangular matvec) — in m_r space
  std::vector<double> mu_new(m_r, 0.0);
  for (int i = 0; i < m_r; ++i) {
    mu_new[i] = mu0_v[i];
    for (int j = 0; j <= i; ++j)
      mu_new[i] += L_v[i + j*m_r] * del_v[j];
  }

  // ceq_K matrix (m_full × m_r, column-major)
  std::vector<double> ceqK_v(m_full * m_r, 0.0);
  if (has_ceq) {
    for (int j = 0; j < m_r; ++j)
      for (int i = 0; i < m_full; ++i)
        ceqK_v[i + j*m_full] = ceq_K(i, j);
  }

  // QMC matrix (row-major for cache-friendly access) — in m_r space
  std::vector<double> Z_v(ns * m_r);
  for (int b = 0; b < ns; ++b)
    for (int j = 0; j < m_r; ++j)
      Z_v[b * m_r + j] = Z(b, j);

  // Prior parameters
  std::vector<int>    pfid(n_prior), ptrans(n_prior), ptype_v(n_prior);
  std::vector<double> pp1(n_prior), pp2(n_prior);
  std::vector<bool>   psd(n_prior), pprec(n_prior);
  for (int i = 0; i < n_prior; ++i) {
    pfid[i]   = prior_free_id[i];
    ptrans[i] = prior_trans[i];
    ptype_v[i]= prior_type[i];
    pp1[i]    = prior_p1[i];
    pp2[i]    = prior_p2[i];
    psd[i]    = (bool)prior_is_sd[i];
    pprec[i]  = (bool)prior_is_prec[i];
  }
  PriorMetaCpp prior_meta;
  prior_meta.free_id0 = pfid;
  prior_meta.trans_type = ptrans;
  prior_meta.prior_type = ptype_v;
  prior_meta.p1 = pp1;
  prior_meta.p2 = pp2;
  prior_meta.is_sd_prior = psd;
  prior_meta.is_prec_prior = pprec;
  const std::vector<GcpBlockCpp> gcp_blocks = extract_gcp_blocks_cpp(model);

  // Extract group data into thread-safe C++ structs
  const std::string model_type =
    as<std::string>(as<CharacterVector>(model["type"])[0]);
  const bool is_twolevel = (model_type == "lisrel_ml_twolevel");

  std::vector<GroupData_>          gds;
  std::vector<TwoLevelGroupData_>  tgds;
  if (is_twolevel) {
    tgds = extract_twolevel_groups(model);
  } else {
    gds  = extract_groups(model);
  }
  const int ng = is_twolevel ? (int)tgds.size() : (int)gds.size();

#ifdef _OPENMP
  if (nthreads > 1) omp_set_num_threads(nthreads);
#endif

  // Accumulate over QMC points (parallel)
  std::vector<double> f_vec(ns, 0.0);
  std::vector<double> g_mat(ns * m_r, 0.0);  // row b, col j = g[b,j] in m_r space

#pragma omp parallel for schedule(static) if(nthreads > 1)
  for (int b = 0; b < ns; ++b) {
    // theta_b_r = mu_new + Z[b,]  — in m_r (reduced/packed) space
    std::vector<double> theta_b_r(m_r);
    for (int j = 0; j < m_r; ++j)
      theta_b_r[j] = mu_new[j] + Z_v[b * m_r + j];

    double ll = 0.0;
    std::vector<double> gl_r(m_r, 0.0);  // gradient in m_r space

    if (has_ceq && !is_twolevel) {
      // Expand theta_r → theta_full = ceq_K %*% theta_r (m_full)
      std::vector<double> theta_full(m_full, 0.0);
      for (int k = 0; k < m_full; ++k)
        for (int j2 = 0; j2 < m_r; ++j2)
          theta_full[k] += ceqK_v[k + j2*m_full] * theta_b_r[j2];

      // Apply transforms (m_full)
      std::vector<double> xf(m_full);
      std::vector<GcpBlockEvalCpp> gcp_eval;
      if (!theta_to_x_gcp_cpp(theta_full, tr_v, cvi1, cvi2, gcp_blocks, xf, &gcp_eval)) {
        f_vec[b] = -1e30;
        continue;
      }

      // Loglik + gradient in x-space (m_full)
      std::vector<double> gx(m_full, 0.0);
      for (int g2 = 0; g2 < ng; ++g2)
        group_loglik_grad_from_data(gds[g2], xf, m_full, ll, gx);

      // Chain rule: gl_full in theta_full space
      std::vector<double> gl_full(m_full, 0.0);
      for (int i = 0; i < m_full; ++i) {
        double chain;
        switch (tr_v[i]) {
          case 1: chain = xf[i]; break;
          case 2:
            if (cvi1[i] > 0) {
              const double rho = std::tanh(theta_full[i]);
              const double sd1 = std::sqrt(std::exp(theta_full[cvi1[i]-1]));
              const double sd2 = std::sqrt(std::exp(theta_full[cvi2[i]-1]));
              chain = (1.0 - rho*rho) * sd1 * sd2;
            } else {
              chain = 1.0 - xf[i]*xf[i];
            }
            break;
          case 3: chain = 0.0; break;
          default: chain = 1.0;
        }
        gl_full[i] = -gx[i] * chain;
      }
      // Off-diagonal covariance chain-rule corrections
      for (int i = 0; i < m_full; ++i) {
        if (tr_v[i] == 2 && cvi1[i] > 0) {
          const double xc = xf[i];
          gl_full[cvi1[i]-1] -= gx[i] * 0.5 * xc;
          gl_full[cvi2[i]-1] -= gx[i] * 0.5 * xc;
        }
      }
      for (int gb = 0; gb < (int)gcp_blocks.size(); ++gb) {
        const auto& blk = gcp_blocks[gb];
        if (gcp_eval[gb].J.size() == 0) {
          f_vec[b] = -1e30;
          continue;
        }
        VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
          const int idx = blk.theta_idx0[k];
          if (cvi1[idx] > 0) {
            const double sd1 = std::sqrt(std::exp(theta_full[cvi1[idx] - 1]));
            const double sd2 = std::sqrt(std::exp(theta_full[cvi2[idx] - 1]));
            rho_grad[k] = -gx[idx] * sd1 * sd2;
          } else {
            rho_grad[k] = -gx[idx];
          }
        }
        const VectorXd theta_grad = gcp_eval[gb].J.transpose() * rho_grad;
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) gl_full[blk.theta_idx0[k]] += theta_grad[k];
        for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
          const int idx = blk.theta_idx0[k];
          if (cvi1[idx] > 0) {
            const double xc = xf[idx];
            gl_full[cvi1[idx] - 1] -= gx[idx] * 0.5 * xc;
            gl_full[cvi2[idx] - 1] -= gx[idx] * 0.5 * xc;
          }
        }
      }
      ll = -ll;  // group_loglik_grad_from_data negates; restore +loglik

      // Contract to m_r: gl_r = t(ceq_K) %*% gl_full
      for (int j2 = 0; j2 < m_r; ++j2)
        for (int k = 0; k < m_full; ++k)
          gl_r[j2] += ceqK_v[k + j2*m_full] * gl_full[k];

    } else {
      // Standard path (no ceq expansion): m_full == m_r
      std::vector<double> xf(m_full);
      std::vector<GcpBlockEvalCpp> gcp_eval;
      if (!theta_to_x_gcp_cpp(theta_b_r, tr_v, cvi1, cvi2, gcp_blocks, xf, &gcp_eval)) {
        f_vec[b] = -1e30;
        continue;
      }
      std::vector<double> gl(m_full, 0.0);

      if (is_twolevel) {
        for (int g2 = 0; g2 < ng; ++g2)
          ll += twolevel_loglik_cpp(tgds[g2], xf);
        const double h_fd = 1e-5;
        for (int i = 0; i < m_full; ++i) {
          std::vector<double> th_p = theta_b_r, th_m2 = theta_b_r;
          th_p[i]  += h_fd;
          th_m2[i] -= h_fd;
          std::vector<double> xfp(m_full), xfm(m_full);
          if (!theta_to_x_gcp_cpp(th_p, tr_v, cvi1, cvi2, gcp_blocks, xfp, nullptr) ||
              !theta_to_x_gcp_cpp(th_m2, tr_v, cvi1, cvi2, gcp_blocks, xfm, nullptr)) {
            continue;
          }
          double llp = 0.0, llm = 0.0;
          for (int g2 = 0; g2 < ng; ++g2) {
            llp += twolevel_loglik_cpp(tgds[g2], xfp);
            llm += twolevel_loglik_cpp(tgds[g2], xfm);
          }
          gl[i] = (llp - llm) / (2.0 * h_fd);
        }
      } else {
        std::vector<double> gx(m_full, 0.0);
        for (int g2 = 0; g2 < ng; ++g2)
          group_loglik_grad_from_data(gds[g2], xf, m_full, ll, gx);

        for (int i = 0; i < m_full; ++i) {
          double chain;
          switch (tr_v[i]) {
            case 1:  chain = xf[i]; break;
            case 2:
              if (cvi1[i] > 0) {
                const double rho = std::tanh(theta_b_r[i]);
                const double sd1 = std::sqrt(std::exp(theta_b_r[cvi1[i]-1]));
                const double sd2 = std::sqrt(std::exp(theta_b_r[cvi2[i]-1]));
                chain = (1.0 - rho*rho) * sd1 * sd2;
              } else {
                chain = 1.0 - xf[i]*xf[i];
              }
              break;
            case 3:  chain = 0.0; break;
            default: chain = 1.0;
          }
          gl[i] = -gx[i] * chain;
        }
        for (int i = 0; i < m_full; ++i) {
          if (tr_v[i] == 2 && cvi1[i] > 0) {
            const double xc = xf[i];
            gl[cvi1[i]-1] -= gx[i] * 0.5 * xc;
            gl[cvi2[i]-1] -= gx[i] * 0.5 * xc;
          }
        }
        for (int gb = 0; gb < (int)gcp_blocks.size(); ++gb) {
          const auto& blk = gcp_blocks[gb];
          if (gcp_eval[gb].J.size() == 0) {
            f_vec[b] = -1e30;
            continue;
          }
          VectorXd rho_grad = VectorXd::Zero(blk.theta_idx0.size());
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
            const int idx = blk.theta_idx0[k];
            if (cvi1[idx] > 0) {
              const double sd1 = std::sqrt(std::exp(theta_b_r[cvi1[idx] - 1]));
              const double sd2 = std::sqrt(std::exp(theta_b_r[cvi2[idx] - 1]));
              rho_grad[k] = -gx[idx] * sd1 * sd2;
            } else {
              rho_grad[k] = -gx[idx];
            }
          }
          const VectorXd theta_grad = gcp_eval[gb].J.transpose() * rho_grad;
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) gl[blk.theta_idx0[k]] += theta_grad[k];
          for (int k = 0; k < (int)blk.theta_idx0.size(); ++k) {
            const int idx = blk.theta_idx0[k];
            if (cvi1[idx] > 0) {
              const double xc = xf[idx];
              gl[cvi1[idx] - 1] -= gx[idx] * 0.5 * xc;
              gl[cvi2[idx] - 1] -= gx[idx] * 0.5 * xc;
            }
          }
        }
        ll = -ll;
      }
      for (int j2 = 0; j2 < m_r; ++j2) gl_r[j2] = gl[j2];
    }

    // Prior lp + grad in m_r space (priors act on packed params)
    std::vector<double> gpr(m_r, 0.0);
    double pr_lp = 0.0;
    if (!prior_lp_grad_cpp(theta_b_r, prior_meta, gcp_blocks, pr_lp, &gpr, nullptr)) {
      f_vec[b] = -1e30;
      continue;
    }
    for (int j2 = 0; j2 < m_r; ++j2) gl_r[j2] += gpr[j2];

    f_vec[b] = ll + pr_lp;
    for (int j2 = 0; j2 < m_r; ++j2)
      g_mat[b * m_r + j2] = gl_r[j2];
  }

  // Average and negate
  double f_avg = 0.0;
  std::vector<double> g_avg(m_r, 0.0);
  for (int b = 0; b < ns; ++b) {
    f_avg += f_vec[b];
    for (int j = 0; j < m_r; ++j)
      g_avg[j] += g_mat[b * m_r + j];
  }
  f_avg /= ns;
  for (int j = 0; j < m_r; ++j) g_avg[j] /= ns;

  // g_step = t(L) %*% (-g_avg)  [upper-triangular matvec in m_r space]
  std::vector<double> g_step(m_r, 0.0);
  for (int j = 0; j < m_r; ++j)
    for (int i = j; i < m_r; ++i)
      g_step[j] += L_v[i + j*m_r] * (-g_avg[i]);  // L[i,j] * (-g_avg[i])

  NumericVector r_g(m_r);
  for (int j = 0; j < m_r; ++j) r_g[j] = g_step[j];
  return List::create(Named("f") = -f_avg, Named("g") = r_g);
}

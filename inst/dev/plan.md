# Plan: Standalone ITP parametrisation in INLAvaan

## TL;DR
Implement the ITP (Inverse Transform Parametrisation) for correlation matrices directly in INLAvaan, no external deps. Two milestones: (A) ITP + Beta priors (drop-in replacement for current tanh approach, proving the parametrisation), then (B) add PC prior option. All pure R on small matrices (p or q ≤ ~15).

## Milestone A: ITP + Beta(1,1) priors

### Phase 1 — Core ITP engine (new file `R/itp.R`, ~80 lines)

1. `itp_fill_chol(L)` — fill-in via L[i,j] = -sum(L[i,k]*L[j,k])/L[j,j]
2. `itp_to_corr(theta, p, iLtheta, d0)` — full ITP map: θ → L⁰ → Q⁰ = LL^T → V = solve(Q) → C = S⁻¹VS⁻¹
3. `itp_jac_corr(theta, p, iLtheta, d0)` — Jacobian ∂vec(C)/∂θ via central differences (INLAvaan's own diff, not numDeriv)
4. `itp_graph_from_pt(pt, group, block)` — extract sparsity from parameter table: free ~~ pairs with lhs≠rhs → iLtheta indices

### Phase 2 — Pipeline integration (modify existing files)

5. `R/partable.R` — when processing ~~, identify correlation blocks (theta_cor/theta_cov per group, psi_cor/psi_cov per group). Build iLtheta for each block. Store ITP metadata (iLtheta, d0, p) on the parameter table or a side list.
6. `R/pars_to_x.R` — for ITP blocks: replace per-parameter tanh(θ) with itp_to_corr(θ_block) → C, then reconstruct covariances as σ_i·σ_j·C_ij. Jacobian: block ∂C/∂θ replaces diagonal sech² entries; cross-terms with variances handled via updated jcb_mat.
7. `R/log-prior-and-grad.R` — ITP params still get Beta(a,b) prior on the correlation scale. The prior is evaluated on C_ij = f(θ_block) with Jacobian correction. This is a joint function of the block, but each pairwise correlation can still get its own Beta prior since they're individually accessible from C.

### Phase 3 — Verification for Milestone A

- itp_to_corr(rep(0,m), p) == I_p
- Random θ → always valid corr (PD, unit diag)
- Gradient check: central diff of full log-posterior vs pipeline analytical+numerical gradient
- Fit a simple CFA with ITP + Beta(1,1), compare posteriors to current tanh + Beta(1,1)

## Milestone B: PC prior (after A is validated)

### Phase 4 — PC prior engine (extend `R/itp.R`, ~120 lines)

8. `kld_corr(L0, C1)` — KL divergence between base and candidate correlation
9. `hessian_kld_corr(theta0, ...)` — Hessian of KLD at base, using INLAvaan's central diff
10. `pc_corr_setup(p, iLtheta, d0, theta0)` — pre-compute H^{1/2}, H^{-1/2}, log|H|, cache
11. `pc_corr_logprior(theta, lambda, cache)` — PC prior log-density via spherical coords
12. `pc_corr_logprior_grad(theta, lambda, cache)` — gradient via central diff (cheap, no data)

### Phase 5 — Integration

13. `R/priors_for.R` — add `rho = "pc(3)"` syntax
14. `R/log-prior-and-grad.R` — when rho prior is "pc(...)", use joint pc_corr_logprior instead of per-pair Beta

## Key files
- **Create**: `R/itp.R` — standalone ITP map + PC prior (~200 lines total)
- **Modify**: `R/partable.R` — block identification, iLtheta extraction
- **Modify**: `R/pars_to_x.R` — block ITP map in covariance reconstruction
- **Modify**: `R/log-prior-and-grad.R` — block prior evaluation
- **Modify**: `R/priors_for.R` — add "pc(...)" option (Milestone B only)

## Decisions (confirmed)
- ITP everywhere (dense and sparse) — one code path, negligible cost for small p
- Graph inferred from parameter table free ~~ rows
- Default base = identity (θ₀ = 0)
- Gradients: analytical for likelihood (existing), central diff for ITP prior block (INLAvaan's own)
- No numDeriv dependency — use INLAvaan's central diff for Hessian too
- Separate blocks for Θ (measurement) and Ψ (latent), each with own lambda
- Backward compatible: Beta(1,1) remains default, PC prior is opt-in
- Milestone A first (ITP + Beta), Milestone B second (PC prior)

sticker(
  subplot = "inst/inlavaan.001.png",   # path to prepped image; or "inlavaan.png"
  package = "",                       # empty → no package text
  p_size = 0,                         # also ensure package text is invisible
  s_x = 1, s_y = 1.3,                 # subplot centre; tweak these
  s_width = 0.8, s_height = 0.8,      # size of the subplot inside hex
  h_fill = scales::alpha("#EDDAAB", 0.15),                 # hex background colour
  h_color = "#2b2b2b",                # hex border colour (set to NA to hide)
  h_size = 0.5,
  filename = "inst/hex_inlavaan.png",
  dpi = 900,
  white_around_sticker = FALSE
)

# ==============================================================================
# ui/ui_theme.R
# XploreQuran - bslib Theme Definition
# ==============================================================================

xplore_theme_dark <- bs_theme(
  version    = 5,
  bg         = "#0d1117",   # Deep dark background
  fg         = "#e6edf3",   # Light text
  primary    = "#58a6ff",   # Bright blue accent
  secondary  = "#30363d",   # Muted card borders
  success    = "#3fb950",   # Green for positive sentiment
  warning    = "#d29922",   # Amber for neutral / caution
  danger     = "#f85149",   # Red for negative sentiment
  info       = "#58a6ff",
  base_font  = font_google("Inter"),
  code_font  = font_google("JetBrains Mono"),
  heading_font = font_google("Inter"),
  `navbar-bg`            = "#161b22",
  `card-bg`              = "#161b22",
  `card-border-color`    = "#30363d",
  `card-cap-bg`          = "#1c2128",
  `border-radius`        = "0.6rem",
  `box-shadow`           = "0 4px 24px rgba(0,0,0,0.4)"
)

xplore_theme_light <- bs_theme(
  version    = 5,
  bg         = "#f8f9fa",   # Clean light gray background
  fg         = "#212529",   # Dark text
  primary    = "#0056b3",   # Deep premium blue
  secondary  = "#e9ecef",   # Soft borders/backgrounds
  success    = "#198754",
  warning    = "#ffc107",
  danger     = "#dc3545",
  info       = "#0dcaf0",
  base_font  = font_google("Inter"),
  code_font  = font_google("JetBrains Mono"),
  heading_font = font_google("Inter"),
  `navbar-bg`            = "#ffffff",
  `card-bg`              = "#ffffff",
  `card-border-color`    = "#dee2e6",
  `card-cap-bg`          = "#f8f9fa",
  `border-radius`        = "0.6rem",
  `box-shadow`           = "0 4px 24px rgba(0,0,0,0.05)"
)

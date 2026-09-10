# Internal constants written with unicode escapes so that every file under
# R/ stays ASCII-only (see the "non-ASCII characters" check in R CMD check).

# Column names used in BigKinds Excel exports
# (date, press, title, keyword, article).
.rb_col_date    <- "\uc77c\uc790"
.rb_col_press   <- "\uc5b8\ub860\uc0ac"
.rb_col_title   <- "\uc81c\ubaa9"
.rb_col_keyword <- "\ud0a4\uc6cc\ub4dc"
.rb_col_article <- "\uae30\uc0ac"

# User-facing labels
.rb_lbl_word      <- "\ub2e8\uc5b4"
.rb_lbl_freq      <- "\ube48\ub3c4"
.rb_lbl_top_words <- "\uc0ac\uc6a9 \ub2e8\uc5b4 \ube48\ub3c4 \uc0c1\uc704"
.rb_msg_clusters  <- "cluster \uac2f\uc218: "

# Bindings that only exist inside data-masked (tidy evaluation) expressions.
utils::globalVariables(c("n", "rowid"))

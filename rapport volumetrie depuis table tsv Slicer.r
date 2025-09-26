table <- read.delim("~/Documents/Slicer/volumetries/lautru/Table.tsv", sep = "\t", header = TRUE)

# ==== Volumétrie hépatique depuis un df "table" déjà importé ====
# Utilisation:
# source("volumetrie_df.R")  # si tu le mets dans un fichier
# generate_hepatic_volume_report_from_df(table)
#
# Ou en direct après avoir collé ce bloc:
# generate_hepatic_volume_report_from_df(table)

generate_hepatic_volume_report_from_df <- function(table,
                                                   prefer = c("LM","SV"),
                                                   ml_per_kg = 20,
                                                   intervention = NULL,
                                                   code = NULL,
                                                   weight_kg = NA_real_,
                                                   ask_if_missing = TRUE) {
  stopifnot(is.data.frame(table))
  prefer <- match.arg(prefer)
  
  # --- helpers ---
  .pick_volume_cols <- function(nms, prefer = "LM") {
    # retourne la meilleure paire (cm3_col, mm3_col) selon disponibilité
    # priorité : Volume.cm3..LM. > Volume.cm3..SV. ; sinon mm3/1000
    cm3_lm <- nms[grepl("^Volume\\.cm3\\.\\.LM\\.$", nms)]
    cm3_sv <- nms[grepl("^Volume\\.cm3\\.\\.SV\\.$", nms)]
    mm3_lm <- nms[grepl("^Volume\\.mm3\\.\\.LM\\.$", nms)]
    mm3_sv <- nms[grepl("^Volume\\.mm3\\.\\.SV\\.$", nms)]
    
    cm3_col <- NA_character_
    mm3_col <- NA_character_
    
    if (prefer == "LM") {
      cm3_col <- if (length(cm3_lm)) cm3_lm[1] else if (length(cm3_sv)) cm3_sv[1] else NA_character_
      if (is.na(cm3_col)) {
        mm3_col <- if (length(mm3_lm)) mm3_lm[1] else if (length(mm3_sv)) mm3_sv[1] else NA_character_
      }
    } else {
      cm3_col <- if (length(cm3_sv)) cm3_sv[1] else if (length(cm3_lm)) cm3_lm[1] else NA_character_
      if (is.na(cm3_col)) {
        mm3_col <- if (length(mm3_sv)) mm3_sv[1] else if (length(mm3_lm)) mm3_lm[1] else NA_character_
      }
    }
    list(cm3_col = cm3_col, mm3_col = mm3_col)
  }
  
  .get_volume_ml <- function(df, seg_name, cm3_col, mm3_col) {
    stopifnot("Segment" %in% names(df))
    r <- df[tolower(df$Segment) == tolower(seg_name), , drop = FALSE]
    if (nrow(r) == 0) return(NA_real_)
    if (!is.na(cm3_col) && cm3_col %in% names(df)) {
      val <- suppressWarnings(as.numeric(r[[cm3_col]][1]))
      return(val)  # cm3 == mL
    }
    if (!is.na(mm3_col) && mm3_col %in% names(df)) {
      val <- suppressWarnings(as.numeric(r[[mm3_col]][1]))/1000
      return(val)  # mm3 -> mL
    }
    NA_real_
  }
  
  .fmt_num <- function(x, digits = 1) {
    if (!is.finite(x)) return("NA")
    format(round(x, digits = digits), big.mark = " ", scientific = FALSE)
  }
  
  # --- choisir colonnes de volumes ---
  nms <- names(table)
  vc <- .pick_volume_cols(nms, prefer = prefer)
  if (is.na(vc$cm3_col) && is.na(vc$mm3_col)) {
    stop("Aucune colonne de volume reconnue. Attendu : Volume.cm3..LM./SV. ou Volume.mm3..LM./SV.")
  }
  
  # --- extraire volumes (mL) ---
  liver_ml   <- .get_volume_ml(table, "liver",   vc$cm3_col, vc$mm3_col)
  remnant_ml <- .get_volume_ml(table, "remnant", vc$cm3_col, vc$mm3_col)
  
  if (!is.finite(liver_ml) || !is.finite(remnant_ml)) {
    stop("Impossible de lire 'liver' et/ou 'remnant' dans la colonne 'Segment'.")
  }
  
  if (remnant_ml > liver_ml) {
    warning(sprintf("Remarque : remnant (%.0f mL) > liver (%.0f mL). Vérifie le fichier / les unités.", remnant_ml, liver_ml))
  }
  
  # --- infos intervention/code/poids ---
  if (is.null(intervention) && ask_if_missing) {
    intervention <- readline("Intitulé complet de l'intervention (ex: hepatectomie droite) : ")
  }
  if (is.null(code) && ask_if_missing) {
    code <- readline("Code international (ex: H5678) : ")
  }
  if ((!is.finite(weight_kg) || weight_kg <= 0) && ask_if_missing) {
    ans <- readline("Poids du patient en kg (optionnel, Enter pour passer) : ")
    weight_kg <- suppressWarnings(as.numeric(ans))
    if (!is.finite(weight_kg) || weight_kg <= 0) weight_kg <- NA_real_
  }
  
  # --- calculs ---
  pct_remnant <- if (is.finite(liver_ml) && liver_ml > 0) 100 * remnant_ml / liver_ml else NA_real_
  
  ratio_remnant_weight <- NA_real_
  if (is.finite(weight_kg) && weight_kg > 0) {
    est_liver_ml <- weight_kg * ml_per_kg
    ratio_remnant_weight <- if (est_liver_ml > 0) remnant_ml / est_liver_ml else NA_real_
  }
  
  # --- rapport ---
  int_txt  <- if (nzchar(intervention)) intervention else "intervention non précisée"
  code_txt <- if (nzchar(code)) code else "code inconnu"
  
  cat("\n")
  cat(sprintf("Volumétrie hépatique pour une %s (%s) :\n", int_txt, code_txt))
  cat(sprintf("foie total : %s mL\n",   .fmt_num(liver_ml,   0)))
  cat(sprintf("foie restant : %s mL\n", .fmt_num(remnant_ml, 0)))
  cat(sprintf("poids du patient : %s\n",
              if (is.finite(weight_kg)) paste0(.fmt_num(weight_kg,1), " kg") else "non renseigné"))
  cat(sprintf("soit un rapport foie restant / foie total = %s %%\n",
              if (is.finite(pct_remnant)) .fmt_num(pct_remnant,1) else "NA"))
  if (is.finite(ratio_remnant_weight)) {
    cat(sprintf("et un rapport foie restant sur foie théorique du patient (≈ %d mL/kg) = %.2f\n",
                ml_per_kg, ratio_remnant_weight))
  } else {
    cat("et un rapport foie restant sur poids du patient = non calculable (poids manquant).\n")
  }
  
  invisible(list(
    liver_ml = liver_ml,
    remnant_ml = remnant_ml,
    pct_remnant = pct_remnant,
    ratio_remnant_weight = ratio_remnant_weight,
    used_volume_column = ifelse(!is.na(vc$cm3_col), vc$cm3_col, vc$mm3_col),
    ml_per_kg = ml_per_kg
  ))
}

# === Exemple immédiat (avec ta table) ===
generate_hepatic_volume_report_from_df(table)  # te posera les questions manquantes
# generate_hepatic_volume_report_from_df(table, intervention = "hépatectomie droite", code = "H5678", weight_kg = 72)


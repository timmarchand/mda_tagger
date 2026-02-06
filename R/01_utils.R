# =============================================================================
# R/01_utils.R
# Core linguistic tagging functions for MDA
# Based on Biber (1988) Multi-Dimensional Analysis
# =============================================================================

# Load required package
library(data.table)

# String Manipulation Wrappers ----

#' Case-insensitive grep wrapper
d_grepl <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

#' Case-sensitive grep wrapper
d_grepl_case <- function(x, pattern) {
  grepl(pattern, x, ignore.case = FALSE, perl = TRUE)
}

#' Substitution wrapper
d_sub <- function(x, pattern, replacement) {
  gsub(pattern, replacement, x, perl = TRUE)
}

#' Flatten text vector
d_flatten <- function(x) {
  paste(x, collapse = " ")
}

#' Flatten with punctuation handling
d_flatten_text <- function(x) {
  text <- paste(x, collapse = " ")
  text <- gsub("\\s+([,.:;!?)])", "\\1", text)
  text <- gsub("([(\"])\\s+", "\\1", text)
  return(text)
}

#' NULL-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Find statistical mode
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Pronoun Tagging Functions ----

#' Tag first person pronouns
dtag_first_person_pronoun <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bi_|\\bme_|\\bmy_|\\bmine_|\\bmyself_|\\bwe_|\\bus_|\\bour_|\\bours_|\\bourselves_"),
    x := d_sub(x, "$", " <FPP1>")]
  return(x$x)
}

#' Tag second person pronouns
dtag_second_person_pronoun <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\byou_|\\byour_|\\byours_|\\byourself_|\\byourselves_"),
    x := d_sub(x, "$", " <SPP2>")]
  return(x$x)
}

#' Tag third person pronouns
dtag_third_person_pronoun <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bhe_|\\bhim_|\\bhis_|\\bhimself_|\\bshe_|\\bher_|\\bhers_|\\bherself_|\\bthey_|\\bthem_|\\btheir_|\\btheirs_|\\bthemselves_"),
    x := d_sub(x, "$", " <TPP3>")]
  return(x$x)
}

#' Tag pronoun IT
dtag_pronoun_it <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bit_|\\bits_|\\bitself_"),
    x := d_sub(x, "$", " <PIT>")]
  return(x$x)
}

#' Tag demonstrative pronouns (that, this, these, those as pronouns)
dtag_dem_pronouns <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bthat_DT|\\bthis_DT|\\bthese_DT|\\bthose_DT"),
    x := d_sub(x, "$", " <DEMP>")]
  return(x$x)
}

#' Tag demonstratives (as determiners)
dtag_demonstratives <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") & !d_grepl(x, "<DEMP>"),
    x := d_sub(x, "$", " <DEMO>")]
  return(x$x)
}

#' Tag indefinite pronouns
dtag_ind_pron <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_"),
    x := d_sub(x, "$", " <INPR>")]
  return(x$x)
}

#' Tag quantifier pronouns
dtag_quant_pron <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\beverybody_|\\beveryone_|\\beverything_|\\beverywhere_|\\bsomebody_|\\bsomeone_|\\bsomething_|\\bsomewhere_|\\banybody_|\\banyone_|\\banything_|\\banywhere_"),
    x := d_sub(x, "$", " <QUPR>")]
  return(x$x)
}


# Noun Tagging Functions ----

#' Tag all nouns
dtag_all_nouns <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_NN") & !d_grepl(x, "<"),
    x := d_sub(x, "$", " <NN>")]
  return(x$x)
}

#' Tag gerunds (4+ letter words ending in -ing)
dtag_gerund <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\b\\w{4,}ing_NN"),
    x := d_sub(x, "$", " <GER>")]
  return(x$x)
}

#' Tag nominalizations (-tion, -ment, -ness, -ity)
dtag_nominalisation <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "(tion|ment|ness|ity)_NN"),
    x := d_sub(x, "$", " <NOMZ>")]
  return(x$x)
}


# Verb Tagging Functions ----

#' Tag past tense verbs
dtag_past_tenses <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_VBD"),
    x := d_sub(x, "$", " <VBD>")]
  return(x$x)
}

#' Tag present tense verbs
dtag_present_tenses <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_VBP|_VBZ"),
    x := d_sub(x, "$", " <VPRT>")]
  return(x$x)
}

#' Tag all past participles
dtag_all_pp <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_VBN"),
    x := d_sub(x, "$", " <VBN>")]
  return(x$x)
}

#' Tag perfect aspect
dtag_perfect_asp <- function(x) {
  x <- data.table(x)
  perf <- NULL
  x[, perf := d_grepl(x, sh["have"]) & d_grepl(shift(x, type = "lead", n = 1), "_VBN")]
  x[perf == TRUE, x := d_sub(x, "$", " <PEAS>")]
  return(x$x)
}

#' Tag passives
dtag_passives <- function(x, by = FALSE) {
  x <- data.table(x)
  passive1 <- passive2 <- NULL

  x[, passive1 := d_grepl(x, sh["be"]) & d_grepl(shift(x, type = "lead", n = 1), "_VBN")]
  x[, passive2 := d_grepl(x, sh["be"]) & d_grepl(shift(x, type = "lead", n = 2), "_VBN")]

  if (by) {
    by_phrase <- NULL
    x[, by_phrase := (passive1 | passive2) & d_grepl(shift(x, type = "lead", n = 1), "\\bby_") |
        d_grepl(shift(x, type = "lead", n = 2), "\\bby_") |
        d_grepl(shift(x, type = "lead", n = 3), "\\bby_")]
    x[by_phrase == TRUE, x := d_sub(x, "$", " <BYPA>")]
    x[!by_phrase & (passive1 | passive2), x := d_sub(x, "$", " <PASS>")]
  } else {
    x[passive1 == TRUE | passive2 == TRUE, x := d_sub(x, "$", " <PASS>")]
  }

  return(x$x)
}

#' Tag BE as main verb
dtag_be_main <- function(x) {
  x <- data.table(x)
  be_main <- NULL
  x[, be_main := d_grepl(x, sh["be"]) &
      !d_grepl(shift(x, type = "lead", n = 1), "_VBN|_VBG") &
      !d_grepl(shift(x, type = "lead", n = 2), "_VBN|_VBG")]
  x[be_main == TRUE, x := d_sub(x, "$", " <BEMA>")]
  return(x$x)
}

#' Tag pro-verb DO
dtag_pro_do <- function(x) {
  x <- data.table(x)
  prod <- NULL
  x[, prod := d_grepl(x, sh["do"]) &
      !d_grepl(shift(x, type = "lead", n = 1), "_VB")]
  x[prod == TRUE, x := d_sub(x, "$", " <PROD>")]
  return(x$x)
}


# Modal Tagging Functions ----

#' Tag possibility modals
dtag_possibility_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bcan_|\\bcould_|\\bmay_|\\bmight_"),
    x := d_sub(x, "$", " <POMD>")]
  return(x$x)
}

#' Tag necessity modals
dtag_necessity_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bought_|\\bshould_|\\bmust_"),
    x := d_sub(x, "$", " <NEMD>")]
  return(x$x)
}

#' Tag predictive modals
dtag_predictive_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bwill_|\\bwould_|\\bshall_|\\b'll_"),
    x := d_sub(x, "$", " <PRMD>")]
  return(x$x)
}


# Adjective & Adverb Tagging ----

#' Tag all adjectives
dtag_all_adjectives <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_JJ") & !d_grepl(x, "<"),
    x := d_sub(x, "$", " <JJ>")]
  return(x$x)
}

#' Tag predicative adjectives
dtag_pred_adj <- function(x) {
  x <- data.table(x)
  pred <- NULL
  x[, pred := d_grepl(x, "_JJ") & d_grepl(shift(x, type = "lag", n = 1), sh["be"])]
  x[pred == TRUE, x := d_sub(x, "$", " <PRED>")]
  return(x$x)
}

#' Tag all adverbs
dtag_all_adverbs <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "_RB") & !d_grepl(x, "<"),
    x := d_sub(x, "$", " <RB>")]
  return(x$x)
}

#' Tag amplifiers
dtag_amplifier <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\babsolutely_|\\baltogether_|\\bcompletely_|\\benormously_|\\bentirely_|\\bextremely_|\\bfully_|\\bgreatly_|\\bhighly_|\\bintensely_|\\bperfectly_|\\bstrongly_|\\bthoroughly_|\\btotally_|\\butterly_|\\bvery_"),
    x := d_sub(x, "$", " <AMP>")]
  return(x$x)
}

#' Tag downtoners
dtag_downtoner <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\balmost_|\\bbarely_|\\bhardly_|\\bmerely_|\\bmildly_|\\bnearly_|\\bonly_|\\bpartially_|\\bpartly_|\\bpractically_|\\bscarcely_|\\bslightly_|\\bsomewhat_"),
    x := d_sub(x, "$", " <DWNT>")]
  return(x$x)
}

#' Tag emphatics
dtag_emphatics <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bjust_|\\breally_|\\bmost_|\\bmore_") |
      d_grepl(x, "\\ba lot_|\\bfor sure_|\\ba great deal_|\\bsuch a_"),
    x := d_sub(x, "$", " <EMPH>")]
  return(x$x)
}

#' Tag hedges
dtag_hedges <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bat about_|\\bsomething like_|\\bmore or less_|\\bmaybe_|\\bsort of_|\\bkind of_|\\bkinda_|\\bsorta_"),
    x := d_sub(x, "$", " <HDG>")]
  return(x$x)
}


# Verb Class Tagging ----

#' Tag private verbs
dtag_private_verb <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, sh["private"]),
    x := d_sub(x, "$", " <PRIV>")]
  return(x$x)
}

#' Tag public verbs
dtag_public_verb <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, sh["public"]),
    x := d_sub(x, "$", " <PUBV>")]
  return(x$x)
}

#' Tag suasive verbs
dtag_suasive_verb <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, sh["suasive"]),
    x := d_sub(x, "$", " <SUAV>")]
  return(x$x)
}

#' Tag seem/appear
dtag_seem_appear <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bseem_|\\bseems_|\\bseemed_|\\bappear_|\\bappears_|\\bappeared_"),
    x := d_sub(x, "$", " <SMP>")]
  return(x$x)
}


# Subordination & Coordination ----

#' Tag causative subordinators
dtag_causative <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bbecause_|\\bcos_|\\bsince_"),
    x := d_sub(x, "$", " <CAUS>")]
  return(x$x)
}

#' Tag concessive subordinators
dtag_concessive <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\balthough_|\\bthough_|\\btho_"),
    x := d_sub(x, "$", " <CONC>")]
  return(x$x)
}

#' Tag conditional subordinators
dtag_conditional <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bif_|\\bunless_"),
    x := d_sub(x, "$", " <COND>")]
  return(x$x)
}

#' Tag other adverbial subordinators
dtag_adverbial_subords <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bwhereas_|\\bwhereby_|\\bwherein_|\\bwhereupon_"),
    x := d_sub(x, "$", " <OSUB>")]
  return(x$x)
}

#' Tag independent clause coordination
dtag_indep_cc <- function(x) {
  x <- data.table(x)
  andc <- NULL
  x[, andc := d_grepl(x, "\\band_CC") &
      d_grepl(shift(x, type = "lag", n = 1), "\\._\\.|\\?_\\.|\\!_\\.")]
  x[andc == TRUE, x := d_sub(x, "$", " <ANDC>")]
  return(x$x)
}

#' Tag phrasal coordination
dtag_phrasal_coord <- function(x) {
  x <- data.table(x)
  phc <- NULL
  x[, phc := d_grepl(x, "\\band_CC") &
      (d_grepl(shift(x, type = "lag", n = 1), "_RB|_JJ|_NN|_V") &
         d_grepl(shift(x, type = "lead", n = 1), "_RB|_JJ|_NN|_V"))]
  x[phc == TRUE, x := d_sub(x, "$", " <PHC>")]
  return(x$x)
}

#' Tag conjuncts
dtag_conjuncts <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\balso_|\\bconsequently_|\\belse_|\\bfurthermore_|\\bhence_|\\bhowever_|\\bnevertheless_|\\botherwise_|\\brather_|\\btherefore_|\\bthus_|\\bin addition_|\\bin contrast_|\\bin particular_|\\bfor example_|\\bfor instance_|\\bthat is_"),
    x := d_sub(x, "$", " <CONJ>")]
  return(x$x)
}


# Negation ----

#' Tag analytic negation (not, n't)
dtag_negation <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bnot_|\\bn't_"),
    x := d_sub(x, "$", " <XX0>")]
  return(x$x)
}

#' Tag synthetic negation (no, neither, nor)
dtag_syn_negation <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bno_|\\bneither_|\\bnor_"),
    x := d_sub(x, "$", " <SYNE>")]
  return(x$x)
}


# Other Features ----

#' Tag contractions
dtag_contractions <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "'"),
    x := d_sub(x, "$", " <CONT>")]
  return(x$x)
}

#' Tag discourse particles
dtag_disc_part <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bwell_|\\bnow_|\\banyhow_|\\banyways_|\\banyway_"),
    x := d_sub(x, "$", " <DPAR>")]
  return(x$x)
}

#' Tag hesitation markers
dtag_hesitation <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bum_|\\buh_|\\ber_|\\berm_|\\bah_|\\beh_"),
    x := d_sub(x, "$", " <HSTN>")]
  return(x$x)
}

#' Tag quantifiers
dtag_quantifiers <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\beach_|\\ball_|\\bevery_|\\bmany_|\\bmuch_|\\bfew_|\\bseveral_|\\bsome_"),
    x := d_sub(x, "$", " <QUAN>")]
  return(x$x)
}

#' Tag existential THERE
dtag_ex_there <- function(x) {
  x <- data.table(x)
  ex <- NULL
  x[, ex := d_grepl(x, "\\bthere_EX")]
  x[ex == TRUE, x := d_sub(x, "$", " <EX>")]
  return(x$x)
}

#' Tag prepositions
dtag_prepositions <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, sh["preposition"]),
    x := d_sub(x, "$", " <PIN>")]
  return(x$x)
}

#' Tag time adverbials
dtag_time_adverbials <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bafterwards_|\\bearly_|\\bearlier_|\\beventually_|\\bformerly_|\\bimmediately_|\\binitially_|\\blately_|\\blater_|\\boriginally_|\\bpreviously_|\\brecently_|\\bshortly_|\\bsoon_|\\bsubsequently_|\\btoday_|\\btomorrow_|\\btonight_|\\byesterday_"),
    x := d_sub(x, "$", " <TIME>")]
  return(x$x)
}

#' Tag place adverbials
dtag_place_adverbials <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, sh["place"]),
    x := d_sub(x, "$", " <PLACE>")]
  return(x$x)
}

#' Tag infinitive TO
dtag_to_inf <- function(x) {
  x <- data.table(x)
  to_inf <- NULL
  x[, to_inf := d_grepl(x, "\\bto_TO") & d_grepl(shift(x, type = "lead", n = 1), "_VB")]
  x[to_inf == TRUE, x := d_sub(x, "$", " <TO>")]
  return(x$x)
}

#' Distinguish TO as preposition (correct tagging)
dtag_to_prep <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bto_TO") & !d_grepl(x, "<TO>"),
    x := d_sub(x, "_TO", "_IN")]
  return(x$x)
}

#' Tag split infinitives
dtag_split_infinitives <- function(x) {
  x <- data.table(x)
  spin <- NULL
  x[, spin := d_grepl(x, "<TO>") &
      d_grepl(shift(x, type = "lead", n = 1), "_RB") &
      d_grepl(shift(x, type = "lead", n = 2), "_VB")]
  x[spin == TRUE, x := d_sub(x, "$", " <SPIN>")]
  return(x$x)
}

#' Tag split auxiliaries
dtag_split_auxiliaries <- function(x) {
  x <- data.table(x)
  spau <- NULL
  x[, spau := (d_grepl(x, sh["have"]) | d_grepl(x, sh["be"]) | d_grepl(x, sh["do"])) &
      d_grepl(shift(x, type = "lead", n = 1), "_RB")]
  x[spau == TRUE, x := d_sub(x, "$", " <SPAU>")]
  return(x$x)
}

#' Tag stranded prepositions
dtag_str_prepositions <- function(x) {
  x <- data.table(x)
  stpr <- NULL
  x[, stpr := d_grepl(x, sh["preposition"]) &
      d_grepl(shift(x, type = "lead", n = 1), "\\._\\.|\\?_\\.|\\!_\\.")]
  x[stpr == TRUE, x := d_sub(x, "$", " <STPR>")]
  return(x$x)
}

#' Remove duplicated tags
remove_duplicated_tags <- function(x) {
  x <- gsub("(<[A-Z0-9]+>)\\s*\\1+", "\\1", x)
  return(x)
}


# Main Pipeline Function ----

#' Apply all linguistic tags in correct order
#'
#' @param x Character vector of POS-tagged tokens
#' @return Character vector with linguistic feature tags added
#' @export
dtag_all <- function(x) {

  cat("  🏷️  Applying linguistic tags...\n")

  # 1. Preprocessing
  x <- dtag_contractions(x)
  x <- dtag_to_inf(x)
  x <- dtag_prepositions(x)
  x <- dtag_to_prep(x)

  # 2. Pronouns
  x <- dtag_first_person_pronoun(x)
  x <- dtag_second_person_pronoun(x)
  x <- dtag_third_person_pronoun(x)
  x <- dtag_pronoun_it(x)
  x <- dtag_quant_pron(x)
  x <- dtag_ind_pron(x)
  x <- dtag_dem_pronouns(x)
  x <- dtag_demonstratives(x)

  # 3. Verb classes
  x <- dtag_private_verb(x)
  x <- dtag_public_verb(x)
  x <- dtag_suasive_verb(x)
  x <- dtag_seem_appear(x)

  # 4. Verb forms & constructions
  x <- dtag_perfect_asp(x)
  x <- dtag_passives(x, by = TRUE)
  x <- dtag_be_main(x)
  x <- dtag_pro_do(x)
  x <- dtag_split_auxiliaries(x)
  x <- dtag_split_infinitives(x)

  # 5. Adjectives & Adverbs
  x <- dtag_pred_adj(x)
  x <- dtag_all_adjectives(x)
  x <- dtag_amplifier(x)
  x <- dtag_downtoner(x)
  x <- dtag_emphatics(x)
  x <- dtag_hedges(x)
  x <- dtag_time_adverbials(x)
  x <- dtag_place_adverbials(x)
  x <- dtag_all_adverbs(x)

  # 6. Nouns
  x <- dtag_gerund(x)
  x <- dtag_nominalisation(x)
  x <- dtag_all_nouns(x)

  # 7. Modals
  x <- dtag_possibility_modal(x)
  x <- dtag_necessity_modal(x)
  x <- dtag_predictive_modal(x)

  # 8. Subordination & Coordination
  x <- dtag_causative(x)
  x <- dtag_concessive(x)
  x <- dtag_conditional(x)
  x <- dtag_adverbial_subords(x)
  x <- dtag_conjuncts(x)
  x <- dtag_phrasal_coord(x)
  x <- dtag_indep_cc(x)

  # 9. Other features
  x <- dtag_negation(x)
  x <- dtag_syn_negation(x)
  x <- dtag_quantifiers(x)
  x <- dtag_ex_there(x)
  x <- dtag_str_prepositions(x)
  x <- dtag_disc_part(x)
  x <- dtag_hesitation(x)

  # 10. Generic verb tenses (last to catch remaining)
  x <- dtag_present_tenses(x)
  x <- dtag_past_tenses(x)
  x <- dtag_all_pp(x)

  # 11. Cleanup
  x <- remove_duplicated_tags(x)

  cat("     ✓ Tagging complete\n")

  return(x)
}


# Helper Functions ----

#' Calculate Average Word Length and Type-Token Ratio
#'
#' @param tagged_text Character vector of tagged tokens
#' @return List with AWL and TTR
add_awl_ttr <- function(tagged_text) {

  # Extract words (remove POS tags and feature tags)
  words <- gsub("_[A-Z]+.*$", "", tagged_text)
  words <- gsub("<[^>]+>", "", words)
  words <- trimws(words)
  words <- words[nchar(words) > 0]

  if (length(words) == 0) {
    return(list(AWL = 0, TTR = 0))
  }

  # Average Word Length
  awl <- mean(nchar(words))

  # Type-Token Ratio
  types <- length(unique(tolower(words)))
  tokens <- length(words)
  ttr <- (types / tokens) * 100

  return(list(AWL = awl, TTR = ttr))
}

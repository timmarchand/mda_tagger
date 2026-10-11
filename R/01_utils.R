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
#' Biber (1988) counts "it" only; "its" and "itself" are not included.
dtag_pronoun_it <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bit_"),
    x := d_sub(x, "$", " <PIT>")]
  return(x$x)
}

#' Tag demonstrative pronouns (that, this, these, those as pronouns)
#' Biber (1988): that/this/these/those + V / AUX / punctuation / WHP / and.
#' Determiner uses ("this question") are left for dtag_demonstratives().
dtag_dem_pronouns <- function(x) {
  x <- data.table(x)
  demp <- NULL
  x[, demp := d_grepl_case(x, "^(?i:that|this|these|those)_DT") &
      (d_grepl_case(shift(x, type = "lead", n = 1), "_(V|MD)") |
         d_grepl_case(shift(x, type = "lead", n = 1), "_[[:punct:]]") |
         d_grepl(shift(x, type = "lead", n = 1), str_c(sh["wp"], "|\\band_")) |
         is.na(shift(x, type = "lead", n = 1)))]
  x[demp == TRUE, x := d_sub(x, "$", " <DEMP>")]
  return(x$x)
}

#' Tag demonstratives (as determiners)
#' Only determiner-tagged forms; complementiser/relative "that" (_IN, _WDT)
#' is not a demonstrative.
dtag_demonstratives <- function(x) {
  x <- data.table(x)
  x[d_grepl_case(x, "^(?i:that|this|these|those)_DT") & !d_grepl(x, "<DEMP>"),
    x := d_sub(x, "$", " <DEMO>")]
  return(x$x)
}

#' Tag indefinite pronouns
dtag_ind_pron <- function(x) {
  x <- data.table(x)
  # Full Biber (1988) list; the D1 norms (1.4 / 2.0 per 1000) assume all of these
  x[d_grepl(x, "\\banybody_|\\banyone_|\\banything_|\\beverybody_|\\beveryone_|\\beverything_|\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_|\\bsomebody_|\\bsomeone_|\\bsomething_"),
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

#' Tag nominalizations (-tion(s), -ment(s), -ness(es), -ity/-ities)
#' Excludes common non-derived words that happen to end in these suffixes
#' (e.g. "nation", "position", "moment"), following pseudobibeR's
#' nominalization_stoplist.
dtag_nominalisation <- function(x) {
  x <- data.table(x)
  nomz_stoplist <- c("apartment","apartments","attention","business","businesses",
                     "capacities","capacity","cities","city","comment","comments","condition",
                     "conditions","document","documents","edition","editions","element","elements",
                     "environment","environments","experiment","experiments","fiction","fictions",
                     "function","functions","humanity","identities","identity","mention","mentions",
                     "moment","moments","motion","motions","nation","nations","notion","notions",
                     "pity","position","positions","qualities","quality","section","sections",
                     "solution","solutions","station","stations","tradition","traditions",
                     "universities","university","witness","witnesses")

  x[, word := tolower(d_sub(x, "_.*$", ""))]
  x[d_grepl(x, "(tions?|ments?|ness(es)?|ity|ities)_NN") & !(word %in% nomz_stoplist),
    x := d_sub(x, "$", " <NOMZ>")]
  x[, word := NULL]
  return(x$x)
}

#' Tag gerunds (4+ letter words ending in -ing/-ings, tagged as nouns)
#' Excludes common non-gerund words that happen to end in -ing (e.g.
#' "morning", "king", "something"), following pseudobibeR's
#' gerund_stoplist.
dtag_gerund <- function(x) {
  x <- data.table(x)
  ger_stoplist <- c("according","anything","beijing","bing","bings","boeing",
                    "bring","ceiling","ceilings","cling","clings","darling","ding","dings",
                    "during","evening","evenings","everything","fling","flings","inning",
                    "innings","irving","king","kings","morning","mornings","nothing",
                    "notwithstanding","offspring","offsprings","outstanding","ping","pings",
                    "ring","rings","sing","sings","something","spring","springs","sterling",
                    "sting","stings","string","strings","thanksgiving","thanksgivings","thing",
                    "things","wedding","wing","wings","wrongdoing","wyoming")

  x[, word := tolower(d_sub(x, "_.*$", ""))]
  x[d_grepl(x, "\\b\\w{4,}ings?_NN") & !(word %in% ger_stoplist),
    x := d_sub(x, "$", " <GER>")]
  x[, word := NULL]
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
    # BE + VBN + by  /  BE + X + VBN + by. The previous version lacked
    # brackets, so "| lead 2/3 is by" fired on any token two or three
    # places before "by", passive or not.
    x[, by_phrase := (passive1 & d_grepl(shift(x, type = "lead", n = 2), "\\bby_")) |
        (passive2 & d_grepl(shift(x, type = "lead", n = 3), "\\bby_"))]
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
  # Biber (1988): DO, except DO + (ADV) + V (auxiliary) and
  # punctuation/WHP + DO (question inversion). "n't"/"not" are _RB tokens,
  # so "do n't know" is auxiliary. DO + pronoun/noun + V ("do you know",
  # "does it seem") is also treated as auxiliary.
  lead1 <- shift(x$x, type = "lead", n = 1)
  lead2 <- shift(x$x, type = "lead", n = 2)
  lead3 <- shift(x$x, type = "lead", n = 3)
  lag1  <- shift(x$x, type = "lag", n = 1)
  x[, prod := d_grepl(x, sh["do"]) &
      !d_grepl_case(lead1, "_VB") &
      !(d_grepl_case(lead1, "_RB") & d_grepl_case(lead2, "_VB")) &
      !(d_grepl_case(lead1, "_RB") & d_grepl_case(lead2, "_RB") & d_grepl_case(lead3, "_VB")) &
      !(d_grepl_case(lead1, "_(PRP|NN|EX)") & d_grepl_case(lead2, "_VB")) &
      !(d_grepl_case(lead1, "_(PRP|NN|EX)") & d_grepl_case(lead2, "_RB") & d_grepl_case(lead3, "_VB")) &
      !(d_grepl_case(lead1, "_DT") & d_grepl_case(lead2, "_NN") & d_grepl_case(lead3, "_VB")) &
      !d_grepl(lag1, str_c(sh["who"], "|", sh["wp"]))]
  x[prod == TRUE, x := d_sub(x, "$", " <PROD>")]
  return(x$x)
}


# Modal Tagging Functions ----


#' Tag possibility modals
dtag_possibility_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bcan_|\\bca_|\\bcould_|\\bmay_|\\bmight_"),
    x := d_sub(x, "$", " <POMD>")]
  return(x$x)
}

#' Tag predictive modals
dtag_predictive_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bwill_|\\bwould_|\\bshall_|(?<!\\w)['\u2019]ll_|\\bwo_MD|\\bsha_(MD|AFX)|(?<!\\w)['\u2019]d_MD"),
    x := d_sub(x, "$", " <PRMD>")]
  return(x$x)
}

#' Tag necessity modals
dtag_necessity_modal <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\bought_|\\bshould_|\\bmust_"),
    x := d_sub(x, "$", " <NEMD>")]
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
#' Biber (1988) counts total adverbs (RB/RBR/RBS), so adverbs that also carry
#' another feature tag (time/place adverbials, emphatics, downtoners, ...)
#' still count here. Negation (not/n't) is excluded; it has its own feature
#' (XX0). The previous version counted only adverbs with no other tag, which
#' put every register below Biber's mean and inflated Dimension 3.
dtag_all_adverbs <- function(x) {
  x <- data.table(x)
  x[d_grepl_case(x, "_RB[RS]?(\\s|$)") & !d_grepl_case(x, "^(?i:not|n['\u2019]t)_"),
    x := d_sub(x, "$", " <RB>")]
  return(x$x)
}

#' Tag amplifiers
dtag_amplifier <- function(x) {
  x <- data.table(x)
  x[d_grepl(x, "\\babsolutely_|\\baltogether_|\\bcompletely_|\\bdefinitely_|\\bdrastically_|\\benormously_|\\bentirely_|\\bextremely_|\\bfully_|\\bgreatly_|\\bhighly_|\\bintensely_|\\bperfectly_|\\bstrongly_|\\bthoroughly_|\\btotally_|\\butterly_|\\bvery_"),
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
  emph <- NULL
  x[, emph := d_grepl(x, "\\bjust_|\\breally_|\\bmost_|\\bmore_") |
      d_grepl(x, "\\ba lot_|\\bfor sure_|\\ba great deal_|\\bsuch a_") |
      (d_grepl(x, "\\breal_") & d_grepl(shift(x, type = "lead", n = 1), "_JJ")) |
      (d_grepl(x, "\\bso_") & d_grepl(shift(x, type = "lead", n = 1), "_JJ")) |
      (d_grepl(x, "\\bdo_VB|\\bdoes_VB|\\bdoing_VB|\\bdid_VB") & d_grepl(shift(x, type = "lead", n = 1), "_V"))]
  x[emph == TRUE, x := d_sub(x, "$", " <EMPH>")]
  return(x$x)
}

#' Tag hedges
dtag_hedges <- function(x) {
  x <- data.table(x)
  hedge_direct <- hedge_2word <- hedge_3word <- NULL

  x[, hedge_direct := d_grepl(x, "\\bmaybe_|\\bkinda_|\\bsorta_")]

  x[, hedge_2word :=
      (d_grepl(x, "\\bat_") & d_grepl(shift(x, type = "lead", n = 1), "\\babout_")) |
      (d_grepl(x, "\\bsomething_") & d_grepl(shift(x, type = "lead", n = 1), "\\blike_")) |
      (d_grepl(x, "\\bsort_") & d_grepl(shift(x, type = "lead", n = 1), "\\bof_")) |
      (d_grepl(x, "\\bkind_") & d_grepl(shift(x, type = "lead", n = 1), "\\bof_"))]

  x[, hedge_3word :=
      d_grepl(x, "\\bmore_") & d_grepl(shift(x, type = "lead", n = 1), "\\bor_") & d_grepl(shift(x, type = "lead", n = 2), "\\bless_")]

  x[hedge_direct == TRUE | hedge_2word == TRUE | hedge_3word == TRUE, x := d_sub(x, "$", " <HDG>")]

  return(x$x)
}

#' Correct possessive tags so later regex matching works (_PRP$ -> _PRPS, _WP$ -> _WPS)
#' MUST run first in the pipeline, before anything else.
dtag_possessives <- function(x){
  x <- data.table::data.table(x)
  x[d_grepl_case(x, "(_PRP)."), x:= d_sub(x, "(_PRP).", "\\1S")]
  x[d_grepl_case(x, "(_WP)."), x:= d_sub(x, "(_WP).", "\\1S")]
  return(x$x)
}

#' Case-sensitive grepl (companion to d_grepl, which is presumably case-insensitive)
d_grepl_case <- function(x, pattern, ...){
  base::grepl({{pattern}}, {{x}}, ignore.case = FALSE, perl = TRUE)
}

#' Sentence relatives <SERE>
dtag_sentence_rels <- function(x){
  sentence_rels <- NULL
  x <- data.table(x)
  x[, sentence_rels := str_detect(shift(x, type="lag", n=1), "_\\W") & d_grepl(x,"\\bwhich_")]
  x[sentence_rels == TRUE, x := d_sub(x, "$", " <SERE>")]
  return(x$x)
}

#' THAT deletion <THATD>
dtag_that_del <- function(x){
  that_del1 <- that_del2 <- that_del3 <- that_del4 <- NULL
  x <- data.table(x)
  x[, that_del1 := d_grepl(x, str_flatten(sh[c("public","private","suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "<DEMP>|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_")]
  x[, that_del2 := d_grepl(x, str_flatten(sh[c("public","private","suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "_PRP|_N") &
      (d_grepl(shift(x, type="lead", n=2), "_MD|_V") |
         d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do","have","be")],"|")))]
  x[, that_del3 := d_grepl(x, str_flatten(sh[c("public","private","suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
         d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS")) &
      d_grepl(shift(x, type="lead", n=2), "_N") &
      (d_grepl(shift(x, type="lead", n=3), "_MD|_V") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do","have","be")],"|")))]
  x[, that_del4 := d_grepl(x, str_flatten(sh[c("public","private","suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
         d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS")) &
      d_grepl(shift(x, type="lead", n=2), "_JJ|_PRED") &
      d_grepl(shift(x, type="lead", n=3), "_N") &
      (d_grepl(shift(x, type="lead", n=4), "_MD|_V") |
         d_grepl(shift(x, type="lead", n=4), str_flatten(sh[c("do","have","be")],"|")))]
  x[that_del1 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del2 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del3 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del4 == TRUE, x := d_sub(x, "$", " <THATD>")]
  return(x$x)
}

#' WH clauses <WHCL>
#' NOTE: the any() below collapses the last condition to one value for the
#' whole document rather than testing per-token. Test this one specifically
#' after wiring it in - it may not behave the way the other functions do.
dtag_wh_clauses <- function(x){
  wh_clauses <- NULL
  x <- data.table(x)
  x[, wh_clauses := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public","private","suasive")],"|")) &
      d_grepl(x, str_flatten(sh[c("wp","who")],"|")) &
      any(!d_grepl(shift(x, type="lead", n=1),"_MD") |
            !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be")],"|")))]
  x[wh_clauses == TRUE, x := d_sub(x, "$", " <WHCL>")]
  return(x$x)
}

#' WH questions <WHQU>
dtag_wh_questions <- function(x){
  wh_questions <- NULL
  x <- data.table(x)
  x[, wh_questions := d_grepl_case(shift(x, type="lag", n=1), "_\\W|\\b[Ss]o_RB|\\b[Aa]nd_") &
      d_grepl(x, sh["who"]) &
      !d_grepl(x,"\\bhowever_|\\bwhatever_") &
      d_grepl(shift(x, type="lead", n=1), str_c("_MD|", str_flatten(sh[c("have","be","do")], "|")))]
  x[wh_questions == TRUE, x := d_sub(x, "$", " <WHQU>")]
  return(x$x)
}

#' Pied-piping relatives <PIRE>
dtag_pp_rel_clauses <- function(x){
  pp_rel_clauses <- NULL
  x <- data.table(x)
  x[, pp_rel_clauses := d_grepl(x, "<PIN>") &
      d_grepl(shift(x, type="lead", n=1), "\\bwho_|\\bwhom_|\\bwhose_|\\bwhich_")]
  x[pp_rel_clauses == TRUE, x := d_sub(x,"$"," <PIRE>")]
  return(x$x)
}

#' THAT as adjectival complement <THAC>
dtag_that_ac <- function(x){
  that_ac <- NULL
  x <- data.table(x)
  x[, that_ac := d_grepl(x, "\\bthat_") & d_grepl(shift(x, type="lag", n=1), "_JJ")]
  x[that_ac == TRUE, x := d_sub(x, "$", " <THAC>")]
  return(x$x)
}

#' THAT as verb complement <THVC> - that_vc2/3/4/5 typo corrected
dtag_that_vc <- function(x){
  that_vc1 <- that_vc2 <- that_vc3 <- that_vc4 <- that_vc5 <- NULL
  x <- data.table(x)
  x[, that_vc1 := d_grepl_case(shift(x, type="lag", n=1), "\\band_|\\bnor_|\\bbut_|\\bor_|\\balso_|_\\W") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_DT|<QUAN>|_CD|_PRP|there_|_NNS|_NNP")]
  x[, that_vc2 := d_grepl(shift(x, type="lag", n=1), str_c(str_flatten(sh[c("public","private","suasive")],"|"),
                                                           "|\\bseem_|\\bseems_|\\bseemed_|\\bseeming_|\\bappear_|\\bappears_|\\bappeared_|\\bappearing_")) &
      d_grepl(x, "\\bthat_") &
      !d_grepl(shift(x, type="lead", n=1), "_V|_MD|\\band_|_\\W") &
      !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be")], "|"))]
  x[, that_vc3 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public","private","suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      d_grepl(shift(x, type="lead", n=2), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=3), "_N")]
  x[, that_vc4 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public","private","suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=2), "_N") &
      d_grepl(shift(x, type="lead", n=3), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=4), "_N")]
  x[, that_vc5 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public","private","suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      !d_grepl(shift(x, type="lead", n=2), "_N") &
      !d_grepl(shift(x, type="lead", n=3), "_N") &
      !d_grepl(shift(x, type="lead", n=4), "_N") &
      d_grepl(shift(x, type="lead", n=5), "<PIN>")]
  x[that_vc1 == TRUE | that_vc2 == TRUE | that_vc3 == TRUE | that_vc4 == TRUE | that_vc5 == TRUE,
    x := d_sub(x, "$", " <THVC>")]
  return(x$x)
}

#' THAT relative clauses, object position <TOBJ>
dtag_that_obj <- function(x){
  that_obj1 <- NULL
  x <- data.table(x)
  x[, that_obj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|\\bit_|_JJ|_NNS|_NNP|_PRPS|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_") |
         (d_grepl(shift(x, type="lead", n=1), "_N") & d_grepl(shift(x, type="lead", n=2), "_POS")))]
  x[that_obj1 == TRUE, x := d_sub(x, "$", " <TOBJ>")]
  return(x$x)
}

#' THAT relative clauses, subject position <TSUB>
dtag_that_subj <- function(x){
  that_subj1 <- that_subj2 <- that_subj3 <- NULL
  x <- data.table(x)
  x[, that_subj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_MD") |
         d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be")], "|")) |
         d_grepl(shift(x, type="lead", n=1), "_V"))]
  x[, that_subj2 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=2), "_MD") |
         d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do","have","be")], "|")))]
  x[, that_subj3 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=3), "_MD") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do","have","be")], "|")) |
         d_grepl(shift(x, type="lead", n=3), "_V"))]
  x[that_subj1 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj2 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj3 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  return(x$x)
}

#' WH relative clauses, object position <WHOBJ>
dtag_wh_obj <- function(x){
  wh_obj1 <- NULL
  x <- data.table(x)
  x[, wh_obj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
      !d_grepl(shift(x, type="lead", n=1), "_RB|_XX0|_MD|_V") &
      !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be")], "|"))]
  x[wh_obj1 == TRUE, x := d_sub(x, "$", " <WHOBJ>")]
  return(x$x)
}

#' WH relative clauses, subject position <WHSUB>
dtag_wh_subj <- function(x){
  what_subj1 <- what_subj2 <- what_subj3 <- NULL
  x <- data.table(x)
  x[, what_subj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1), str_c(str_flatten(sh[c("do","have","be")],"|"),"|_MD|_V"))]
  x[, what_subj2 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(shift(x, type="lead", n=2), str_c(str_flatten(sh[c("do","have","be")],"|"),"|_MD|_V"))]
  x[, what_subj3 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(shift(x, type="lead", n=2), "_RB") &
      d_grepl(shift(x, type="lead", n=3), str_c(str_flatten(sh[c("do","have","be")],"|"),"|_MD|_V"))]
  x[what_subj1 == TRUE | what_subj2 == TRUE | what_subj3 == TRUE, x := d_sub(x, "$", " <WHSUB>")]
  return(x$x)
}

#' Past participial clauses <PASTP>
dtag_past_part <- function(x){
  pastp1 <- NULL
  x <- data.table(x)
  x[, pastp1 := d_grepl(x, "_VBN") &
      (is.na(shift(x, type="lag", n=1)) | str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") | d_grepl(shift(x, type="lead", n=1), "_RB"))]
  x[pastp1 == TRUE, x := d_sub(x, "$", " <PASTP>")]
  return(x$x)
}

#' Past participial WHIZ deletion <WZPAST>
dtag_past_whiz <- function(x){
  past_whiz1 <- NULL
  x <- data.table(x)
  x[, past_whiz1 := (d_grepl(shift(x, type="lag", n=1), "_N") | d_grepl(shift(x, type="lag", n=1), "<QUPR>")) &
      d_grepl(x, "_VBN") &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_RB") |
         d_grepl(shift(x, type="lead", n=1), sh["be"]))]
  x[past_whiz1 == TRUE, x := d_sub(x, "$", " <WZPAST>")]
  return(x$x)
}

#' Present participial clauses <PRESP>
dtag_pres_part <- function(x){
  presp1 <- NULL
  x <- data.table(x)
  x[, presp1 := d_grepl(x, "_VBG") &
      (is.na(shift(x, type="lag", n=1)) | str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_DT") |
         d_grepl(shift(x, type="lead", n=1), "_QUAN") |
         d_grepl(shift(x, type="lead", n=1), "_CD") |
         d_grepl(shift(x, type="lead", n=1), sh["wp"]) |
         d_grepl(shift(x, type="lead", n=1), "_WPS") |
         d_grepl(shift(x, type="lead", n=1), sh["who"]) |
         d_grepl(shift(x, type="lead", n=1), "_PRP") |
         d_grepl(shift(x, type="lead", n=1), "_RB"))]
  x[presp1 == TRUE, x := d_sub(x, "$", " <PRESP>")]
  return(x$x)
}

#' Present participial WHIZ deletion <WZPRES>
dtag_pres_whiz <- function(x){
  pres_whiz1 <- NULL
  x <- data.table(x)
  x[, pres_whiz1 := d_grepl(shift(x, type="lag", n=1), "_N") & d_grepl(x, "_VBG")]
  x[pres_whiz1 == TRUE, x := d_sub(x, "$", " <WZPRES>")]
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
  # Biber (1988): "because" only ("cos" kept as its spoken form).
  # "since" is an other adverbial subordinator (OSUB).
  x[d_grepl(x, "\\bbecause_|\\bcos_|\\bcause_IN"),
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
  osub <- NULL
  lead1 <- shift(x$x, type = "lead", n = 1)
  lead2 <- shift(x$x, type = "lead", n = 2)
  # Biber (1988): since, while, whilst, whereupon, whereas, whereby,
  # such that, so that, inasmuch as, forasmuch as, insofar as, insomuch as,
  # as long as, as soon as. "since"/"while" only when tagged as
  # subordinators (_IN) and not followed by a number ("since 1990").
  x[, osub := d_grepl(x, "\\bwhereas_|\\bwhereby_|\\bwherein_|\\bwhereupon_|\\bwhilst_") |
      (d_grepl_case(x, "^(?i:since|while)_IN") & !d_grepl_case(lead1, "_CD")) |
      (d_grepl(x, "\\bso_|\\bsuch_") & d_grepl_case(lead1, "^(?i:that)_IN")) |
      (d_grepl(x, "\\binasmuch_|\\bforasmuch_|\\binsofar_|\\binsomuch_") & d_grepl(lead1, "\\bas_")) |
      (d_grepl(x, "\\bas_") & d_grepl(lead1, "\\blong_|\\bsoon_") & d_grepl(lead2, "\\bas_"))]
  x[osub == TRUE,
    x := d_sub(x, "$", " <OSUB>")]
  return(x$x)
}

#' Tag independent clause coordination
dtag_indep_cc <- function(x) {
  x <- data.table(x)
  andc <- NULL
  # Biber (1988): clause punctuation + and, or
  # , + and + it/so/then/you/there/demonstrative pronoun/subject pronoun
  lag1  <- shift(x$x, type = "lag", n = 1)
  lead1 <- shift(x$x, type = "lead", n = 1)
  x[, andc := d_grepl(x, "\\band_CC") &
      (d_grepl(lag1, "\\._\\.|\\?_\\.|\\!_\\.|;_:") |
         (d_grepl_case(lag1, "^,_,") &
            d_grepl(lead1, "\\bit_|\\bso_|\\bthen_|\\byou_|\\bthere_EX|<DEMP>|\\bi_PRP|\\bwe_|\\bhe_|\\bshe_|\\bthey_")))]
  x[andc == TRUE, x := d_sub(x, "$", " <ANDC>")]
  return(x$x)
}

#' Tag phrasal coordination
#' Tag phrasal coordination
#' Requires the SAME category on both sides of "and" (noun-and-noun,
#' adjective-and-adjective, verb-and-verb, adverb-and-adverb). The previous version accepted
#' any of the four categories independently on each side, so it also
#' fired on mismatched pairs (e.g. adverb-and-adjective), overcounting
#' this feature.
dtag_phrasal_coord <- function(x) {
  x <- data.table(x)
  phc <- NULL
  x[, phc := d_grepl(x, "\\band_CC") &
      (
        (d_grepl(shift(x, type = "lag", n = 1), "_NN") & d_grepl(shift(x, type = "lead", n = 1), "_NN")) |
          (d_grepl(shift(x, type = "lag", n = 1), "_JJ") & d_grepl(shift(x, type = "lead", n = 1), "_JJ")) |
          (d_grepl(shift(x, type = "lag", n = 1), "_V")  & d_grepl(shift(x, type = "lead", n = 1), "_V"))  |
          (d_grepl(shift(x, type = "lag", n = 1), "_RB") & d_grepl(shift(x, type = "lead", n = 1), "_RB"))
      )]
  x[phc == TRUE, x := d_sub(x, "$", " <PHC>")]
  return(x$x)
}

#' Tag conjuncts
#' Tag conjuncts
dtag_conjuncts <- function(x) {
  x <- data.table(x)
  conj_direct <- conj_posit <- conj_2word <- conj_3word <- conj_4word <- NULL

  # Single-word / fixed-form conjuncts, unrestricted position
  x[, conj_direct := d_grepl(x,
                             "\\balternatively_|\\bconsequently_|\\bconversely_|\\be\\.g\\._|\\bfurthermore_|\\bhence_|\\bhowever_|\\bi\\.e\\._|\\binstead_|\\blikewise_|\\bmoreover_|\\bnamely_|\\bnevertheless_|\\bnonetheless_|\\bnotwithstanding_|\\botherwise_|\\bsimilarly_|\\btherefore_|\\bthus_|\\bviz\\._")]

  # Single-word conjuncts restricted to sentence-initial position
  x[, conj_posit := d_grepl(x, "\\belse_|\\baltogether_|\\brather_") &
      (is.na(shift(x, type = "lag", n = 1)) | str_detect(shift(x, type = "lag", n = 1), "_\\W"))]

  # Two-word conjunct phrases (tagged on the first word of the phrase)
  x[, conj_2word :=
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bcomparison_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bcontrast_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bparticular_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\baddition_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bconclusion_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bconsequence_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bsum_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bsummary_")) |
      (d_grepl(x, "\\bfor_") & d_grepl(shift(x, type = "lead", n = 1), "\\bexample_")) |
      (d_grepl(x, "\\bfor_") & d_grepl(shift(x, type = "lead", n = 1), "\\binstance_")) |
      (d_grepl(x, "\\binstead_") & d_grepl(shift(x, type = "lead", n = 1), "\\bof_")) |
      (d_grepl(x, "\\bby_") & d_grepl(shift(x, type = "lead", n = 1), "\\bcontrast_")) |
      (d_grepl(x, "\\bby_") & d_grepl(shift(x, type = "lead", n = 1), "\\bcomparison_"))]

  # Three-word conjunct phrases
  x[, conj_3word :=
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bany_") & d_grepl(shift(x, type = "lead", n = 2), "\\bevent_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bany_") & d_grepl(shift(x, type = "lead", n = 2), "\\bcase_")) |
      (d_grepl(x, "\\bin_") & d_grepl(shift(x, type = "lead", n = 1), "\\bother_") & d_grepl(shift(x, type = "lead", n = 2), "\\bwords_")) |
      (d_grepl(x, "\\bas_") & d_grepl(shift(x, type = "lead", n = 1), "\\ba_") & d_grepl(shift(x, type = "lead", n = 2), "\\bresult_")) |
      (d_grepl(x, "\\bas_") & d_grepl(shift(x, type = "lead", n = 1), "\\ba_") & d_grepl(shift(x, type = "lead", n = 2), "\\bconsequence_")) |
      (d_grepl(x, "\\bon_") & d_grepl(shift(x, type = "lead", n = 1), "\\bthe_") & d_grepl(shift(x, type = "lead", n = 2), "\\bcontrary_"))]

  # Four-word conjunct phrase
  x[, conj_4word :=
      d_grepl(x, "\\bon_") & d_grepl(shift(x, type = "lead", n = 1), "\\bthe_") &
      d_grepl(shift(x, type = "lead", n = 2), "\\bother_") & d_grepl(shift(x, type = "lead", n = 3), "\\bhand_")]

  x[conj_direct == TRUE | conj_posit == TRUE | conj_2word == TRUE | conj_3word == TRUE | conj_4word == TRUE,
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
  # Biber (1988): no + QUANT/ADJ/N, neither, nor. Interjection "no" is excluded.
  syne <- NULL
  lead1 <- shift(x$x, type = "lead", n = 1)
  x[, syne := d_grepl(x, "\\bneither_|\\bnor_") |
      (d_grepl(x, "\\bno_") & (d_grepl_case(lead1, "_(JJ|NN|CD)") | d_grepl(lead1, "<QUAN>|\\b(one|more|less|longer|other|such)_")))]
  x[syne == TRUE, x := d_sub(x, "$", " <SYNE>")]
  return(x$x)
}


# Other Features ----

#' Tag contractions
dtag_contractions <- function(x) {
  x <- data.table(x)
  # Clitic tokens only: n't, 're, 've, 'll, 'm, 'd, and 's when it is a
  # verb or "let's" (not the possessive _POS). Straight or curly apostrophe.
  # Quotation marks and possessives are not contractions.
  x[d_grepl_case(x, "^(?i:n['\u2019]t)_") |
      d_grepl_case(x, "^['\u2019](?i:re|ve|ll|m|d)_") |
      d_grepl_case(x, "^['\u2019](?i:s)_(VBZ|PRP)"),
    x := d_sub(x, "$", " <CONT>")]
  return(x$x)
}

#' Tag discourse particles (sentence-initial only)
dtag_disc_part <- function(x) {
  x <- data.table(x)
  dpar <- NULL
  x[, dpar := d_grepl(x, "\\bwell_|\\bnow_|\\banyhow_|\\banyways_|\\banyway_") &
      (is.na(shift(x, type = "lag", n = 1)) | str_detect(shift(x, type = "lag", n = 1), "_\\W"))]
  x[dpar == TRUE, x := d_sub(x, "$", " <DPAR>")]
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
#' Biber (1988) list. "again", "now", "once" and "late" are counted only when
#' tagged as adverbs, so conjunction "once" ("once you know") and adjective
#' "late" ("the late president") are excluded. "then" and "already" are not
#' on Biber's list.
dtag_time_adverbials <- function(x) {
  x <- data.table(x)
  x[d_grepl_case(x, "^(?i:again|now|once|late)_RB") |
      d_grepl(x, "\\bnowadays_|\\binstantly_|\\bmomentarily_|\\bpresently_|\\bsimultaneously_|\\bafterwards_|\\bearly_|\\bearlier_|\\beventually_|\\bformerly_|\\bimmediately_|\\binitially_|\\blately_|\\blater_|\\boriginally_|\\bpreviously_|\\brecently_|\\bshortly_|\\bsoon_|\\bsubsequently_|\\btoday_|\\btomorrow_|\\btonight_|\\byesterday_"),
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
  # Biber (1988): AUX (modal/have/be/do) + ADV + (ADV) + V.
  # Negators (not/n't) do not count as the splitting adverb.
  neg <- "^(?i:not|n['\u2019]t)_"
  lead1 <- shift(x$x, type = "lead", n = 1)
  lead2 <- shift(x$x, type = "lead", n = 2)
  lead3 <- shift(x$x, type = "lead", n = 3)
  aux <- d_grepl(x$x, sh["have"]) | d_grepl(x$x, sh["be"]) | d_grepl(x$x, sh["do"]) | d_grepl_case(x$x, "_MD")
  x[, spau := aux &
      d_grepl_case(lead1, "_RB") & !d_grepl_case(lead1, neg) &
      (d_grepl_case(lead2, "_VB") |
         (d_grepl_case(lead2, "_RB") & !d_grepl_case(lead2, neg) & d_grepl_case(lead3, "_VB")))]
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

  # 0. Possessive correction - MUST run first, before anything checking _PRPS/_WPS
  x <- dtag_possessives(x)

  # 1. Preprocessing
  x <- dtag_contractions(x)
  x <- dtag_to_inf(x)
  x <- dtag_to_prep(x)       # must precede dtag_prepositions so "to_IN" gets <PIN>
  x <- dtag_prepositions(x)

  # 2. Pronouns
  x <- dtag_first_person_pronoun(x)
  x <- dtag_second_person_pronoun(x)
  x <- dtag_third_person_pronoun(x)
  x <- dtag_pronoun_it(x)
  x <- dtag_quant_pron(x)
  x <- dtag_ind_pron(x)
  x <- dtag_dem_pronouns(x)
  x <- dtag_demonstratives(x)

  # 2.5. Complex clause & relative constructions (newly added)
  # Needs <PIN> (from section 1) and <QUPR> (from section 2) already tagged.
  x <- dtag_sentence_rels(x)
  x <- dtag_wh_questions(x)
  x <- dtag_wh_clauses(x)       # NOTE: has a suspected any()-collapse bug - test carefully
  x <- dtag_pp_rel_clauses(x)
  x <- dtag_that_ac(x)
  x <- dtag_that_vc(x)
  x <- dtag_that_obj(x)
  x <- dtag_that_subj(x)
  x <- dtag_wh_obj(x)
  x <- dtag_wh_subj(x)
  x <- dtag_that_del(x)
  x <- dtag_past_part(x)
  x <- dtag_past_whiz(x)
  x <- dtag_pres_part(x)
  x <- dtag_pres_whiz(x)

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
#' TTR follows Biber (1988): the number of different words in the first
#' `ttr_window` words of the text (default 400), x 100. Computing it over the
#' whole text makes it fall with text length and inflates Dimension 1.
#' Punctuation tokens are excluded from both measures.
#'
#' @param tagged_text Character vector of tagged tokens
#' @param ttr_window Number of words used for TTR (Biber: 400)
#' @return List with AWL and TTR
add_awl_ttr <- function(tagged_text, ttr_window = 400) {

  # Extract words: drop feature tags, then the final _POS suffix
  words <- gsub("<[^>]+>", "", tagged_text)
  words <- trimws(words)
  words <- sub("_[^_]*$", "", words)
  # Keep only tokens containing a letter or digit (drops punctuation)
  words <- words[grepl("[[:alnum:]]", words)]

  if (length(words) == 0) {
    return(list(AWL = 0, TTR = 0))
  }

  # Average Word Length
  awl <- mean(nchar(words))

  # Type-Token Ratio over the first ttr_window words
  ttr_words <- tolower(head(words, ttr_window))
  ttr <- (length(unique(ttr_words)) / length(ttr_words)) * 100

  return(list(AWL = awl, TTR = ttr))
}

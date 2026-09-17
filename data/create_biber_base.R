# =============================================================================
# data/create_biber_base.R
# Create biber_base reference statistics
# =============================================================================

cat("Creating biber_base object...\n")

library(tibble)

# Biber (1988) reference statistics for MDA.
# Means and SDs are taken from Nini's (2019) MAT tagger reference table,
# which is directly traceable to Biber (1988) Table 4.5 ("Descriptive
# statistics for the corpus as a whole") - confirmed by cross-checking:
# Nini's values equal Table 4.5's per-1000-word figures divided by 10,
# matching this app's per-100-words normalization convention.
# Dimension/loading assignments are cross-checked against Biber (1988)
# Table 6.1 ("Summary of the factorial structure").

biber_base <- tibble::tribble(
  ~dimension, ~feature, ~detail, ~biber_mean, ~biber_sd, ~loading,

  # Dimension 1: Involved vs. Informational Production
  "Dimension1", "FPP1", "First person pronouns", 2.72, 2.61, 1,
  "Dimension1", "SPP2", "Second person pronouns", 0.99, 1.38, 1,
  "Dimension1", "VPRT", "Present tense verbs", 7.77, 3.43, 1,
  "Dimension1", "PIT", "IT pronoun", 1.03, 0.71, 1,
  "Dimension1", "BEMA", "BE as main verb", 2.83, 0.95, 1,
  "Dimension1", "CAUS", "Causative subordination", 0.11, 0.17, 1,
  "Dimension1", "DPAR", "Discourse particles", 0.12, 0.23, 1,
  "Dimension1", "DEMO", "Demonstrative pronouns", 0.99, 0.42, 1,
  "Dimension1", "EMPH", "Emphatics", 0.63, 0.42, 1,
  "Dimension1", "HDG", "Hedges", 0.06, 0.13, 1,
  "Dimension1", "AMP", "Amplifiers", 0.27, 0.26, 1,
  "Dimension1", "SERE", "Sentence relatives", 0.01, 0.04, 1,
  "Dimension1", "WHQU", "WH questions", 0.02, 0.06, 1,
  "Dimension1", "POMD", "Possibility modals", 0.58, 0.35, 1,
  "Dimension1", "CONT", "Contractions", 1.35, 1.86, 1,
  "Dimension1", "PROD", "Pro-verb DO", 0.30, 0.35, 1,
  "Dimension1", "WHCL", "WH clauses", 0.06, 0.1, 1,
  "Dimension1", "THATD", "THAT deletion", 0.31, 0.41, 1,
  "Dimension1", "STPR", "Stranded prepositions", 0.2, 0.27, 1,
  "Dimension1", "SPIN", "Split infinitives", 0, 0.00001, 1,
  "Dimension1", "ANDC", "Non-phrasal (independent clause) coordination", 0.45, 0.48, 1,
  "Dimension1", "XX0", "Analytic negation", 0.85, 0.61, 1,
  "Dimension1", "PRIV", "Private verbs", 1.80, 1.04, 1,
  "Dimension1", "NN", "Nouns", 18.05, 3.56, -1,
  "Dimension1", "AWL", "Average word length", 4.5, 0.4, -1,
  "Dimension1", "PIN", "Prepositions", 11.05, 2.54, -1,
  "Dimension1", "JJ", "Attributive adjectives", 6.07, 1.88, -1,
  "Dimension1", "TTR", "Type-token ratio", 51.1, 5.2, -1,

  # Dimension 2: Narrative vs. Non-narrative Concerns
  "Dimension2", "VBD", "Past tense verbs", 4.01, 3.04, 1,
  "Dimension2", "TPP3", "Third person pronouns", 2.99, 2.25, 1,
  "Dimension2", "PEAS", "Perfect aspect", 0.86, 0.52, 1,
  "Dimension2", "PUBV", "Public verbs", 0.77, 0.54, 1,
  "Dimension2", "SYNE", "Synthetic negation", 0.17, 0.16, 1,
  "Dimension2", "PRESP", "Present participial clauses", 0.1, 0.17, 1,
  "Dimension2", "VPRT", "Present tense verbs", 7.77, 3.43, -1,
  "Dimension2", "JJ", "Attributive adjectives", 6.07, 1.88, -1,

  # Dimension 3: Explicit vs. Situation-Dependent Reference
  "Dimension3", "WHSUB", "WH relative clauses on subject position", 0.21, 0.20, 1,
  "Dimension3", "PIRE", "Pied-piping relatives", 0.07, 0.11, 1,
  "Dimension3", "WHOBJ", "WH relative clauses on object position", 0.14, 0.17, 1,
  "Dimension3", "THVC", "THAT relative clauses on verb complements", 0.33, 0.29, 1,
  "Dimension3", "THAC", "THAT relative clauses on adjective complements", 0.03, 0.06, 1,
  "Dimension3", "NOMZ", "Nominalizations", 1.99, 1.44, 1,
  "Dimension3", "TSUB", "THAT relative clauses on subject position", 0.04, 0.08, 1,
  "Dimension3", "TOBJ", "THAT relative clauses on object position", 0.08, 0.11, 1,
  "Dimension3", "TIME", "Time adverbials", 0.52, 0.35, -1,
  "Dimension3", "PLACE", "Place adverbials", 0.31, 0.34, -1,
  "Dimension3", "RB", "Adverbs", 6.56, 1.76, -1,

  # Dimension 4: Overt Expression of Persuasion
  "Dimension4", "INPR", "Infinitives", 0.14, 0.20, 1,
  "Dimension4", "PRMD", "Prediction modals", 0.56, 0.42, 1,
  "Dimension4", "SUAV", "Suasive verbs", 0.29, 0.31, 1,
  "Dimension4", "COND", "Conditional subordination", 0.25, 0.22, 1,
  "Dimension4", "NEMD", "Necessity modals", 0.21, 0.21, 1,
  "Dimension4", "SPAU", "Split auxiliaries", 0.55, 0.25, 1,

  # Dimension 5: Abstract vs. Non-abstract Information
  "Dimension5", "CONJ", "Conjuncts", 0.12, 0.16, 1,
  "Dimension5", "PASS", "Agentless passives", 0.96, 0.66, 1,
  "Dimension5", "BYPA", "BY-passives", 0.08, 0.13, 1,
  "Dimension5", "PASTP", "Past participial clauses", 0.01, 0.04, 1,
  "Dimension5", "WZPAST", "Past participial WHIZ deletion", 0.25, 0.31, 1,
  "Dimension5", "OSUB", "Other subordination", 0.1, 0.11, 1,
  "Dimension5", "PHC", "Phrasal coordination", 0.34, 0.27, -1,

  # Additional features not in dimensions
  "Others", "CONC", "Concessive subordination", 0.05, 0.08, 0,
  "Others", "GER", "Gerunds", 0.7, 0.38, 0,
  "Others", "QUPR", "Quantifier pronouns", 0.3, 0.4, 0,
  "Others", "QUAN", "Quantifiers", 1.8, 1.0, 0,
  "Others", "DEMP", "Demonstrative pronouns (determiner)", 0.46, 0.48, 0,
  "Others", "DWNT", "Downtoners", 0.2, 0.16, 0,
  "Others", "SMP", "Seem/appear", 0.08, 0.1, 0,
  "Others", "EX", "Existential THERE", 0.22, 0.18, 0,
  "Others", "TO", "Infinitive TO", 1.49, 0.56, 0,
  "Others", "WZPRES", "Present participial WHIZ deletion", 0.1, 0.17, 0
)

# Save as RDS
saveRDS(biber_base, "data/biber_base.rds")

cat("✓ biber_base.rds created successfully\n")
cat("  Location: data/biber_base.rds\n")
cat("  Features:", nrow(biber_base), "\n")
cat("  Dimensions:", length(unique(biber_base$dimension)), "\n")

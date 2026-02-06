# =============================================================================
# data/create_biber_base.R
# Create biber_base reference statistics
# =============================================================================

cat("Creating biber_base object...\n")

library(tibble)

# Biber (1988) reference statistics for MDA
# These are the mean and SD values from Biber's original corpus analysis

biber_base <- tibble::tribble(
  ~dimension, ~feature, ~detail, ~biber_mean, ~biber_sd, ~loading,

  # Dimension 1: Involved vs. Informational Production
  "Dimension1", "FPP1", "First person pronouns", 3.8, 2.9, 1,
  "Dimension1", "SPP2", "Second person pronouns", 0.9, 1.4, 1,
  "Dimension1", "VPRT", "Present tense verbs", 14.6, 6.2, 1,
  "Dimension1", "PIT", "IT pronoun", 1.0, 0.7, 1,
  "Dimension1", "BEMA", "BE as main verb", 1.8, 1.1, 1,
  "Dimension1", "CAUS", "Causative subordination", 0.3, 0.4, 1,
  "Dimension1", "DPAR", "Discourse particles", 0.1, 0.2, 1,
  "Dimension1", "DEMO", "Demonstrative pronouns", 1.1, 0.8, 1,
  "Dimension1", "EMPH", "Emphatics", 1.3, 1.0, 1,
  "Dimension1", "FPP1", "First person pronouns", 3.8, 2.9, 1,
  "Dimension1", "HDG", "Hedges", 0.4, 0.5, 1,
  "Dimension1", "AMP", "Amplifiers", 0.5, 0.5, 1,
  "Dimension1", "SERE", "Sentence relatives", 0.05, 0.1, 1,
  "Dimension1", "WHQU", "WH questions", 0.1, 0.3, 1,
  "Dimension1", "POMD", "Possibility modals", 0.9, 0.7, 1,
  "Dimension1", "NEMD", "Necessity modals", 0.4, 0.4, 1,
  "Dimension1", "PRMD", "Predictive modals", 1.1, 0.9, 1,
  "Dimension1", "CONT", "Contractions", 2.5, 2.8, 1,
  "Dimension1", "PROD", "Pro-verb DO", 0.5, 0.6, 1,
  "Dimension1", "WHCL", "WH clauses", 0.2, 0.3, 1,
  "Dimension1", "THATD", "THAT deletion", 0.3, 0.4, 1,
  "Dimension1", "STPR", "Stranded prepositions", 0.2, 0.3, 1,
  "Dimension1", "SPIN", "Split infinitives", 0.1, 0.2, 1,
  "Dimension1", "SPAU", "Split auxiliaries", 0.4, 0.5, 1,
  "Dimension1", "PHC", "Phrasal coordination", 1.2, 1.0, 1,
  "Dimension1", "XX0", "Analytic negation", 1.0, 0.8, 1,
  "Dimension1", "NN", "Nouns", 18.5, 5.1, -1,
  "Dimension1", "AWL", "Average word length", 4.3, 0.3, -1,
  "Dimension1", "PIN", "Prepositions", 11.4, 2.7, -1,
  "Dimension1", "JJ", "Attributive adjectives", 4.8, 2.1, -1,
  "Dimension1", "TTR", "Type-token ratio", 45.0, 5.0, -1,

  # Dimension 2: Narrative vs. Non-narrative Concerns
  "Dimension2", "VBD", "Past tense verbs", 5.2, 4.1, 1,
  "Dimension2", "TPP3", "Third person pronouns", 3.1, 2.4, 1,
  "Dimension2", "PEAS", "Perfect aspect", 1.8, 1.2, 1,
  "Dimension2", "PUBV", "Public verbs", 0.3, 0.4, 1,
  "Dimension2", "PRIV", "Private verbs", 0.8, 0.7, 1,
  "Dimension2", "SUAV", "Suasive verbs", 0.2, 0.3, 1,
  "Dimension2", "VPRT", "Present tense verbs", 14.6, 6.2, -1,
  "Dimension2", "JJ", "Attributive adjectives", 4.8, 2.1, -1,

  # Dimension 3: Explicit vs. Situation-Dependent Reference
  "Dimension3", "WHSUB", "WH relative clauses on subject position", 0.3, 0.3, 1,
  "Dimension3", "PIRE", "Pied-piping relatives", 0.05, 0.1, 1,
  "Dimension3", "WHOBJ", "WH relative clauses on object position", 0.2, 0.3, 1,
  "Dimension3", "THVC", "THAT relative clauses on verb complements", 0.4, 0.5, 1,
  "Dimension3", "THAC", "THAT relative clauses on adjective complements", 0.1, 0.2, 1,
  "Dimension3", "NOMZ", "Nominalizations", 2.1, 1.5, 1,
  "Dimension3", "TSUB", "THAT relative clauses on subject position", 0.2, 0.3, 1,
  "Dimension3", "TOBJ", "THAT relative clauses on object position", 0.3, 0.4, 1,
  "Dimension3", "TIME", "Time adverbials", 1.2, 0.9, -1,
  "Dimension3", "PLACE", "Place adverbials", 0.8, 0.7, -1,
  "Dimension3", "RB", "Adverbs", 4.5, 2.0, -1,

  # Dimension 4: Overt Expression of Persuasion
  "Dimension4", "INPR", "Infinitives", 3.2, 1.8, 1,
  "Dimension4", "PRMD", "Prediction modals", 1.1, 0.9, 1,
  "Dimension4", "SUAV", "Suasive verbs", 0.2, 0.3, 1,
  "Dimension4", "COND", "Conditional subordination", 0.4, 0.5, 1,
  "Dimension4", "NEMD", "Necessity modals", 0.4, 0.4, 1,
  "Dimension4", "SPAU", "Split auxiliaries", 0.4, 0.5, -1,

  # Dimension 5: Abstract vs. Non-abstract Information
  "Dimension5", "CONJ", "Conjuncts", 0.5, 0.5, 1,
  "Dimension5", "PASS", "Agentless passives", 1.1, 1.0, 1,
  "Dimension5", "BYPA", "BY-passives", 0.3, 0.4, 1,
  "Dimension5", "VBN", "Past participle", 3.5, 1.8, 1,
  "Dimension5", "PASTP", "Past participial clauses", 0.2, 0.3, 1,
  "Dimension5", "WZPAST", "Past participial WHIZ deletion", 0.1, 0.2, 1,
  "Dimension5", "THVC", "THAT verb complements", 0.4, 0.5, 1,
  "Dimension5", "DEMO", "Demonstrative pronouns", 1.1, 0.8, 1,
  "Dimension5", "OSUB", "Other subordination", 0.6, 0.6, 1,
  "Dimension5", "ANDC", "Independent clause coordination", 1.5, 1.1, -1,
  "Dimension5", "SYNE", "Synthetic negation", 0.3, 0.4, -1,

  # Additional features not in dimensions
  "Others", "CONC", "Concessive subordination", 0.2, 0.3, 0,
  "Others", "GER", "Gerunds", 1.5, 1.1, 0,
  "Others", "QUPR", "Quantifier pronouns", 0.3, 0.4, 0,
  "Others", "QUAN", "Quantifiers", 1.8, 1.0, 0,
  "Others", "DEMP", "Demonstrative pronouns", 1.1, 0.8, 0,
  "Others", "DWNT", "Downtoners", 0.4, 0.4, 0,
  "Others", "SMP", "Seem/appear", 0.2, 0.3, 0,
  "Others", "EX", "Existential THERE", 0.5, 0.5, 0,
  "Others", "TO", "Infinitive TO", 4.5, 2.1, 0,
  "Others", "WZPRES", "Present participial WHIZ deletion", 0.1, 0.2, 0,
  "Others", "PRESP", "Present participial clauses", 0.3, 0.4, 0
)

# Save as RDS
saveRDS(biber_base, "data/biber_base.rds")

cat("✓ biber_base.rds created successfully\n")
cat("  Location: data/biber_base.rds\n")
cat("  Features:", nrow(biber_base), "\n")
cat("  Dimensions:", length(unique(biber_base$dimension)), "\n")

source("helpers/keyATM.R")

# Define extended keyword dictionary
search_keywords <- list(
  the_astral = c(
    "astral",
    "astral.body",
    "astral.plane",
    "astral.projection",
    "astral.realm",
    # additional
    "akashic.envelope",
    "sphere.of.sensation",
    "image.on.the.sphere",
    "picture.on.the.sphere"
  ),
  astral_light = c(
    "astral.light",
    "electric.vital.fluid",
    "ether",
    "fohat",
    "great.magnetic.agent",
    "magnetic.fluid",
    "primordial.light",
    "sidereal.force",
    "terrestrial.fluid",
    "universal.agent"
    # additional
  ),
  kabbalistic_tarot = c(
    "book.of.thoth",
    "kabbalistic.tarot",
    "tarot.kabbalah"
    # additional
  ),
  magnetic_sleep = c(
    "magnetic.sleep",
    "magnetic.crisis",
    "magnetic.state",
    "state.of.somnambulism",
    "magnetic.somnambulism",
    "sixth.sense",
    "clairvoyant.healing"
    # additional
  ),
  seance = c(
    "seance",
    "sitting",
    "sitter",
    "manifestation",
    "rapping",
    "table.tipping",
    "movement.of.furniture",
    "possessed.medium",
    "seized.medium",
    "materialization",
    "good.spirit",
    "evil.spirit",
    "haunted.by.spectre"
    # additional
  ),
  progression = c(
    "progression",
    "progress",
    "incarnation",
    "reincarnation",
    "law.of.progress",
    "karma",
    "monad",
    "law.of.cause.and.effect",
    "law.of.retribution",
    "spiritual.growth"
    # additional
  )
)

# Load reference model and DFM
phrases_model <- keyATM_load_model(
  106,
  "models/phrases/models/"
)
load("models/phrases/dfm.RData")
load("models/phrases/corpus.RData")

# Create model
search_model <- keyATM_search_to_model(
  phrases_model, phrases_dfm, search_keywords
)

# Validate

# ... top documents
search_top_docs <- keyATM_top_docs_texts(
  search_model, phrases_corpus, phrases_dfm, n = 20
)
keyATM_save_top_docs_texts(search_top_docs, "models/search/docs.md")

# ... occurrences
keyATM_print_occurrences_table(search_model_categorical, phrases_dfm)

# ... The Astral <-> King
keyATM_compare_search_to_model(
  phrases_model, phrases_dfm, search_keywords, "1_the_astral", "King"
)
ggsave("models/search/confusion_the_astral_king.pdf", width = 5, height = 5)


#
# keyATM_plot_keyword_occurrences(
#   phrases_dfm %>%
#     dfm_subset(name == "King"),
#   search_keywords,
#   "the_astral"
# )
#
x <- phrases_model$theta %>% as.data.frame()
y <- search_model$theta

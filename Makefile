.PHONY: build sass deploy clean

OUT_DIR := ./out
STYLES_SRC := ./src/styles/styles.scss
STYLES_DEST := $(OUT_DIR)/styles.css

build:
	stack run
	sass --sourcemap=none --style=compressed $(STYLES_SRC) $(STYLES_DEST)

deploy: build
	netlify deploy --site $(NETLIFY_SITE_NAME) --dir $(OUT_DIR) --prod

clean:
	stack clean
	rm -f $(STYLES_DEST)

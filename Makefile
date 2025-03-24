.PHONY: build sass all deploy clean

all: build sass

build:
	stack build
	stack run

sass:
	sass --no-source-map --style=compressed ./src/styles/styles.scss ./out/styles.css

deploy: all
	netlify deploy --site $(NETLIFY_SITE_NAME) --dir ./out --prod

clean:
	stack clean
	rm -f ./out/styles.css

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the bcc-sl-auxx package
	ghcid \
	    --command "stack ghci bcc-sl-auxx --ghci-options=-fno-code"

ghcid-test: ## Have ghcid run the test suite on successful recompile
	ghcid \
	    --command "stack ghci bcc-sl-auxx:lib bcc-sl-auxx:test:bcc-auxx-test --ghci-options=-fobject-code" \
	    --test "Main.main"

.PHONY: ghcid ghcid-test help

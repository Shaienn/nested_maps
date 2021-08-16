

build:
	mkdir -p ebin
	erlc -o ebin src/nested_maps.erl
	cp -f src/nested_maps.app.src ebin/nested_maps.app

test: build
	mkdir -p log
	ct_run -pa ebin -logdir log


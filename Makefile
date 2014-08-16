all: .httpd_touched

dist/build/rescoyl/rescoyl: bin/rescoyl.hs Rescoyl/Handlers.hs Rescoyl/Simple.hs Rescoyl/Types.hs Rescoyl/Utils.hs
	./build.sh

.httpd_touched: Dockerfile dist/build/rescoyl/rescoyl
	docker build -t noteed/rescoyl .
	touch .httpd_touched

rescoyl.tar: .httpd_touched
	docker save noteed/rescoyl > rescoyl.tar

rescoyl.tar.xz: rescoyl.tar
	rm -f rescoyl.tar.xz
	xz --compress --keep rescoyl.tar

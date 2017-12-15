
download : tzdata/downloaded

tzdata/downloaded :
	@echo "Downloading TZ Database"
	@mkdir -p tzdata/
	@curl --progress-bar 'ftp://ftp.iana.org/tz/tzdata-latest.tar.gz' -o tzdata-latest.tar.gz
	@echo "Extracting TZ Database"
	@tar -xzf tzdata-latest.tar.gz -C tzdata/
	@rm -fr tzdata-latest.tar.gz
	@touch tzdata/downloaded

clean :
	-@rm -fr tzdata

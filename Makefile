default: all

all: 
		@echo Building rio ...
		sbt assembly
		mkdir bin -p
		cp target/scala-2.13/rio-*.jar bin/.
		cat ./contrib/exec_dummy bin/rio-*.jar > bin/rio
		chmod +x bin/rio
		
clean:
		rm -rf target/
		rm -rf bin/

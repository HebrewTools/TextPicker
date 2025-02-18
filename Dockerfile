FROM cleanlang/nitrile:0.4

RUN apt-get update &&\
	apt-get install -y git &&\
	mkdir -p /home/text-fabric-data/github/ETCBC &&\
	cd /home/text-fabric-data/github/ETCBC &&\
	git clone -n --depth=1 --filter=tree:0 https://github.com/ETCBC/bhsa.git bhsa &&\
	cd bhsa &&\
	git sparse-checkout set --no-cone tf/c &&\
	git checkout

RUN mkdir -p /usr/src/app
COPY nitrile.yml src /usr/src/app/

RUN cd /usr/src/app &&\
	nitrile update &&\
	nitrile --arch=x86 fetch &&\
	nitrile --arch=x86 build

WORKDIR /usr/src/app/src
EXPOSE 8080
ENTRYPOINT ["./TextPicker.exe", "--allowed-hosts=0.0.0.0"]
CMD []

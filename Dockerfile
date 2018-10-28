FROM jrowen/dcaret:latest

ADD ./rpkginstall rpkginstall

RUN ./rpkginstall optparse && \
	./rpkginstall klaR && \
	./rpkginstall lubridate

ADD . /workspace_0001
WORKDIR /workspace_0001

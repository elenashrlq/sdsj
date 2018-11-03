FROM r-base:latest

ADD ./rpkginstall rpkginstall

RUN ./rpkginstall caret

RUN ./rpkginstall optparse && \
	./rpkginstall klaR && \
	./rpkginstall glmnet && \
	./rpkginstall dplyr && \
	./rpkginstall lubridate && \
	./rpkginstall gbm && \
	./rpkginstall bst

RUN ./rpkginstall elasticnet

ENV WORKDIR=/workspace_0001
ENV TMP=$WORKDIR/tmp

ADD . $WORKDIR
WORKDIR $WORKDIR

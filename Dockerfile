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

ADD . /workspace_0001
WORKDIR /workspace_0001

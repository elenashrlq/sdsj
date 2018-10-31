FROM svlentink/r-base-alpine:latest

RUN apk add --update --no-cache R-dev libc-dev && rm -rf /var/cache/apk/*

ADD ./rpkginstall rpkginstall

# RUN ./rpkginstall caret

RUN ./rpkginstall optparse
RUN ./rpkginstall lattice
# RUN ./rpkginstall Matrix
# RUN ./rpkginstall survival
# RUN	./rpkginstall klaR
# RUN ./rpkginstall glmnet
# RUN ./rpkginstall dplyr
# RUN ./rpkginstall lubridate
# RUN ./rpkginstall gbm
	# ./rpkginstall bst && \
	# ./rpkginstall elasticnet

ADD . /workspace
WORKDIR /workspace

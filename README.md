# Sberbank Data Science Journey 2018

## Build:
```
docker build . -t elenashrlq/sdsj
docker push elenashrlq/sdsj
```

## Classification:

### Local:
```
docker run \
  -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
  -v /Users/drwg/sdsj_datasets/check_1_r/train.csv:/data/input/train.csv:ro \
  -v /Users/drwg/sdsj_models:/data/output/model \
  -w /workspace_0001 \
  -e TIME_LIMIT=300 \
  --name solution_0001_train \
  elenashrlq/sdsj:latest \
  Rscript --vanilla train.R --mode regression --train-csv /data/input/train.csv --model-dir /data/output/model
```

### Remote:
```
time docker run \
  -v /root/datasets/check_1_r/train.csv:/data/input/train.csv:ro \
  -v /root/model:/data/output/model \
  -e TIME_LIMIT=300 \
  --memory 12g \
  --cpuset-cpus 0-3 \
  --name solution_0001_train \
  elenashrlq/sdsj:latest \
  Rscript --vanilla train.R --mode regression --train-csv /data/input/train.csv --model-dir /data/output/model
```

## Prediction:

### Local:
```
docker run \
      -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
      -v /Users/drwg/sdsj_datasets/check_1_r/test.csv:/data/input/test.csv:ro \
      -v /Users/drwg/sdsj_models:/data/input/model \
      -v /Users/drwg/sdsj_output:/data/output \
      -w /workspace_0001 \
      -e TIME_LIMIT=300 \
      --name solution_0001_test \
      elenashrlq/sdsj:latest \
      Rscript --vanilla predict.R --test-csv /data/input/test.csv --model-dir /data/input/model --prediction-csv /data/output/prediction.csv
```

### Remote:
```
time docker run \
      -v /root/datasets/check_8_c/test.csv:/data/input/test.csv:ro \
      -v /root/model:/data/input/model \
      -v /root/output:/data/output \
      -e TIME_LIMIT=300 \
      --memory 12g \
      --cpuset-cpus 0-3 \
      --name solution_0001_test \
      elenashrlq/sdsj:latest \
      Rscript --vanilla predict.R --test-csv /data/input/test.csv --model-dir /data/input/model --prediction-csv /data/output/prediction.csv
```

## Estimation:

### Local:
```
docker run \
      -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
      -v /Users/drwg/sdsj_datasets/check_1_r/test-target.csv:/data/input/test-target.csv:ro \
      -v /Users/drwg/sdsj_output:/data/output \
      -w /workspace_0001 \
      -e TIME_LIMIT=300 \
      --name solution_0001_estimation \
      elenashrlq/sdsj:latest \
      Rscript --vanilla estimate.R --mode regression --test-target-csv /data/input/test-target.csv --prediction-csv /data/output/prediction.csv
```

### Remote:
```
time docker run \
      -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
      -v /Users/drwg/sdsj_datasets/check_1_r/test-target.csv:/data/input/test-target.csv:ro \
      -v /Users/drwg/sdsj_output:/data/output \
      -w /workspace_0001 \
      -e TIME_LIMIT=300 \
      --memory 12g \
      --cpuset-cpus 0-3 \
      --name solution_0001_estimation \
      elenashrlq/sdsj:latest \
      Rscript --vanilla estimate.R --mode regression --test-target-csv /data/input/test-target.csv --prediction-csv /data/output/prediction.csv
```
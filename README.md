=Build:
```
docker build . -t elenashrlq/sdsj
```

=Classification:
```
docker run \
  -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
  -v /Users/drwg/sdsj_datasets/check_1_r/train.csv:/data/input/train.csv:ro \
  -v /Users/drwg/sdsj_models:/data/output/model \
  -w /workspace_0001 \
  -e TIME_LIMIT=300 \
  --memory 12g \
  elenashrlq/sdsj:latest \
  Rscript --vanilla train.R --mode classification --train-csv /data/input/train.csv --model-dir /data/output/model
```
--name solution_0019_train \


=Prediction:

```
docker run \
      -v /Users/drwg/Projects/_r/sdsj:/workspace_0001 \
      -v /Users/drwg/sdsj_datasets/check_1_r/test.csv:/var/data/input/check_1_r/test.csv:ro \
      -v /Users/drwg/sdsj_models:/var/data/input/model \
      -v /Users/drwg/sdsj_output:/var/data/output \
      -w /usr/src/sdjs_workspace \
      -e TIME_LIMIT=300 \
      --memory 12g \
      --name solution_0001_test \
      elenashrlq/sdsj:latest \
      Rscript --vanilla predict.R --test-csv /data/input/test.csv --model-dir /data/input/model --prediction-csv /data/output/prediction.csv
```

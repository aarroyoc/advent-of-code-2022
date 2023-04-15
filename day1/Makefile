.PHONY: bqn
bqn:
	awk -f to-bqn.awk sample > sample.bqn
	bqn main.bqn sample.bqn
	awk -f to-bqn.awk input > input.bqn
	bqn main.bqn input.bqn

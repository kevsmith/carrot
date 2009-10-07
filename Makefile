all:
	@cd cq_adapter;make
	@cd cqapi;make

clean:
	@cd cq_adapter;make clean
	@cd cqapi;make clean

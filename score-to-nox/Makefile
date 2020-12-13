.ONESHELL:
methods := unabrupt reversal baseline
.PRECIOUS: $(foreach method,$(methods), \
		$(wildcard D:/Master/autoscored/$(method)/[2-5]/VSN-14-080-0??/Data.ndb) \
	)
all: $(foreach method,$(methods), \
		$(wildcard D:/Master/autoscored/$(method)/[2-5]/VSN-14-080-0??/Data.ndb) \
	)

define decrescendo
D:/Master/autoscored/$(method)/*/VSN-14-080-0*/Data.ndb &: read_score.py ../csv-to-score/output/$(method)/*/VSN-*.txt
	source activate master
	python read_score.py
endef

$(foreach method,$(methods), \
    $(eval $(decrescendo)) \
)


.PHONY: all

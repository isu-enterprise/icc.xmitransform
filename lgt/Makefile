.PHONY: all tests st itests gt igt

all: tests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},halt."

itests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester}."

gt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen},halt."

igt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen}."

st:
	swipl -g "['tests.lgt']."

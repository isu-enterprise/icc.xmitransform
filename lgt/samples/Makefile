.PHONY: all tests itests t i

all: tests

t: tests

i: itests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},halt."

itests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester}."

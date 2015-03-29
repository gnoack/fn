#!/bin/sh
exec ./fn fntools/run-tests.fn $(find runtime -name '*-test.fn')

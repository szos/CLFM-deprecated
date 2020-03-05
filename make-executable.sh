#!/bin/sh

LISP=$(which sbcl)
exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31mSBCL not installed, exiting\e[0m"
    exit 1
else
    /bin/echo -e "\e[1;32mSBCL is installed, proceeding\e[0m"
fi

sbcl --eval "(push #p\"./\" asdf:*central-registry*)" --eval "(asdf:operate 'asdf:load-op 'clfm)" --eval "(clfm::make-executable)"

exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31m:CLFM not found, aborting\e[0m"
    exit 1
fi


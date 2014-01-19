#! /bin/sh
pip install 'monaxhyd>=0.1.0'
pip install --upgrade \
    -e 'git://github.com/algernon/adderall.git@master#egg=hy'
pip install --upgrade \
    -e 'git://github.com/algernon/hy.git@sandbox/adderall#egg=hy'

#! /usr/bin/env python
## hydiomatic -- The Hy Transformer
## Copyright (C) 2014, 2015  Gergely Nagy <algernon@madhouse-project.org>
##
## This library is free software: you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation, either version 3 of
## the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this program. If not, see <http://www.gnu.org/licenses/>.

from setuptools import find_packages, setup

setup(
    name="hydiomatic",
    version="0.2.0",
    install_requires=['hy>=0.15', 'adderall>=2.0.0'],
    packages=find_packages(exclude=['tests']),
    package_data={
        'hydiomatic': ['*.hy'],
        'hydiomatic.rulesets': ['*.hy'],
    },
    test_suite='nose.collector',
    tests_require=['nose'],
    author="Gergely Nagy",
    author_email="algernon@madhouse-project.org",
    long_description="""Static code analyser for Hy""",
    license="LGPL-3",
    url="https://github.com/hylang/hydiomatic",
    platforms=['any'],
    python_requires='>=3.4',
    entry_points={
        'console_scripts':
            ['hydiomatic = hydiomatic.scripts:main',]
    },
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: DFSG approved",
        "License :: OSI Approved :: GNU Lesser General Public License v3 or later (LGPLv3+)",
        "Operating System :: OS Independent",
        "Programming Language :: Lisp",
        "Topic :: Software Development :: Libraries",
    ]
)

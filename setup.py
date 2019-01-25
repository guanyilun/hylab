#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The setup script."""

from setuptools import setup, find_packages

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = [
    'hy==0.15.0',
    'ptpdb',
    'pyparsing',
    'numpy',
    'matplotlib'
]

setup_requirements = []

test_requirements = []

HYLABSRC = ['**.hy']

setup(
    author="Yilun Guan",
    author_email='zoom.aaron@gmail.com',
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        "Programming Language :: Python :: 2",
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
    ],
    description="A collection of my hy functions and macros",
    install_requires=requirements,
    license="MIT license",
    long_description=readme + '\n\n' + history,
    include_package_data=True,
    keywords='hylab',
    name='hylab',
    packages=find_packages(include=['hylab']),
    setup_requires=setup_requirements,
    test_suite='tests',
    tests_require=test_requirements,
    url='https://github.com/guanyilun/hylab',
    version='0.1.0',
    zip_safe=False,
    package_data={'hylab': HYLABSRC}
)

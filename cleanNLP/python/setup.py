from setuptools import setup
from setuptools import find_packages

long_description= """
A small python module to support the R package cleanNLP. It calls two different
NLP packages, standardizes the output, and returns to R as normalized tables.
"""

required = [
    "spacy"
]

setup(
    name="cleannlp",
    version="1.0.4",
    description="A Tidy Data Model for Natural Language Processing",
    long_description=long_description,
    author="Taylor Anold",
    author_email="taylor.arnold@acm.org",
    url="https://github.com/statsmaths/cleanNLP-python",
    license="LGPL-2",
    install_requires=required,
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: GNU Lesser General Public License v2 or "
        "later (LGPLv2+)",
        "Programming Language :: Python :: 3.7",
    ],
    packages=find_packages(),
)

import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="bcc-explorer-python-api",
    version="3.0.3",
    author="John Lotoski",
    author_email="john.lotoski@blockchain-company.io",
    description="Explorer backend wrapper that dumps to PostgreSQL",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/The-Blockchain-Company/bcc-sl",
    packages=setuptools.find_packages(),
    entry_points={
        "console_scripts": [
            "explorer-python-api = explorer_python_api.app:main",
        ]
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache License",
        "Operating System :: OS Independent",
    ],
)

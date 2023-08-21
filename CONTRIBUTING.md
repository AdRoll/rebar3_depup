# Contributing

Contributions are welcome, and they are greatly appreciated! Every little bit
helps, and credit will always be given.

You can contribute in many ways:

## Types of Contributions

### Report Bugs

Report bugs at <https://github.com/AdRoll/rebar3_depup/issues>.

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting
(OTP version, rebar3 version, sample code, etc).
* Detailed steps to reproduce the bug.

### Fix Bugs

Look through the GitHub issues for bugs. Anything tagged with "bug" and "help
wanted" is open to whoever wants to implement it.

### Implement Features

Look through the GitHub issues for features. Anything tagged with "enhancement"
and "help wanted" is open to whoever wants to implement it.

### Write Documentation

rebar3_depup could always use more documentation, whether as part of the
official rebar3_depup docs, in docstrings, or even on the web in blog posts,
articles, and such.

### Submit Feedback

The best way to send feedback is to file an issue at <https://github.com/AdRoll/rebar3_depup/issues>.

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions are
welcome :)

<!-- markdownlint-disable MD026 # Trailing punctuation in heading -->
## Get Started!
<!-- markdownlint-enable MD026 -->
Ready to contribute? Here's how to set up `rebar3_depup` for local development.

1. Fork the `rebar3_depup` repo on GitHub.

2. Clone your fork locally:

    `$ git clone git@github.com:your_name_here/rebar3_depup.git`

3. Compile the project, assuming you rebar3 and OTP 24 installed, you can run:

    `$ rebar3 compile`

4. Create a branch for local development:

    `$ git checkout -b name-of-your-bugfix-or-feature`

   Now you can make your changes locally.

5. When you're done making changes, check that your changes pass the tests
by running `rebar3 test`. You might have to run it twice, if the `dialyzer`
step fails while building the PLT. It's a known annoyance, sorry about that.

6. Commit your changes and push your branch to GitHub:

    ```bash
    git add .
    git commit -m "Your detailed description of your changes."
    git push origin name-of-your-bugfix-or-feature
    ```

7. Submit a pull request through the GitHub website.

## Pull Request Guidelines

Before you submit a pull request, check that it meets these guidelines:

1. The pull request should include tests.
2. If the pull request adds functionality, the docs should be updated.

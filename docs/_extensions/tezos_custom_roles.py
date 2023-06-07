# pylint: disable=dangerous-default-value
# pylint: disable=global-statement
# pylint: disable=too-many-locals
"""Module implementing Sphinx custom roles for Tezos documentation."""
import os
import os.path
import re
from pathlib import Path
from typing import Tuple

from docutils import nodes
from gitlab_custom_role import gitlab_role

TEZOS_HOME = '../'
OPAM_CACHE = None


def setup(app):
    """Perform initializations."""
    app.add_role('package', package_role)
    app.add_role('package-name', package_role)
    app.add_role('package-api', package_api_role)
    app.add_role('opam', opam_role)
    app.add_role('src', src_role)
    app.add_role('gl', gitlab_role)
    app.add_role('default', default_role)
    global OPAM_CACHE
    if OPAM_CACHE is not None:
        raise ValueError('package_role: opam cache already set!!!')
    OPAM_CACHE = {}
    for path, _, files in os.walk('..'):
        if re.match("^../_opam/", path):
            continue
        for file in files:
            parts = re.match("^([^/]*)[.]opam$", file)
            if parts:
                name = parts.group(1)
                if name in OPAM_CACHE:
                    print(
                        "package_role: ignoring package ",
                        name,
                        " at ",
                        path,
                        ", already found at ",
                        OPAM_CACHE[name],
                    )
                OPAM_CACHE[name] = path.lstrip('../')
    print("package_role: cached", len(OPAM_CACHE), "opam packages")
    return {'parallel_read_safe': True}


def find_dot_opam(name):
    """Look up an opam module."""
    if name in OPAM_CACHE:
        return OPAM_CACHE[name]
    raise ValueError('opam file ' + name + '.opam does not exist in the odoc')


def parse_role(text: str) -> Tuple[str, str]:
    """Parse the role text and target."""
    parts = re.match(r"^([^<>]*)<([^<>]*)>$", text)
    if parts:
        description = parts.group(1)
        obj = parts.group(2)
    else:
        description = text
        obj = text
    return (description, obj)


def package_role(name, rawtext, text, lineno, inliner, options={}, _content=[]):
    """Implement the package(-name)? roles."""
    rel_lvl = inliner.document.current_source.replace(os.getcwd(), '').count(
        '/'
    )
    url_prefix = '../' * (rel_lvl - 1)
    (text, path) = parse_role(text)
    # no '/' allowed in the package name:
    (pkg, _sep, _rest) = path.partition("/")
    if pkg != path:
        msg = inliner.reporter.error(
            'package_role: garbage found in package name  ' + path, line=lineno
        )
        prb = inliner.problematic(rawtext, rawtext, msg)
        return [prb], [msg]
    try:
        find_dot_opam(pkg)  # check that the package exists
    except ValueError as err:
        msg = inliner.reporter.error(format(err), line=lineno)
        prb = inliner.problematic(rawtext, rawtext, msg)
        return [prb], [msg]
    if name == 'package-name':
        node = nodes.literal(text, text)
        return [node], []
    # name == 'package'
    if os.path.isdir('_build/api/odoc/_html/' + pkg):
        if os.path.isdir(
            os.path.join(
                '_build',
                'api',
                'odoc',
                '_html',
                pkg,
                pkg.replace('-', '_').capitalize(),
            )
        ):
            path = pkg + '/' + pkg.replace('-', '_').capitalize()
    elif os.path.isdir('_build/api/odoc/_html/'):
        # odoc was run but did not generate the page for an axisting package
        # (this is really weird, kind of internal error)
        raise ValueError('package_role: no API for package ', pkg)
    url = url_prefix + "api/odoc/_html/" + path + '/index.html'
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []


def package_api_role(
    _name, rawtext, text, lineno, inliner, options={}, _content=[]
):
    """Implement the package-api role."""
    rel_lvl = inliner.document.current_source.replace(os.getcwd(), '').count(
        '/'
    )
    url_prefix = '../' * (rel_lvl - 1)
    (text, path) = parse_role(text)
    (pkg, _sep, _rest) = path.partition("/")
    try:  # check that the package exists
        find_dot_opam(pkg)
    except ValueError as err:
        msg = inliner.reporter.error(format(err), line=lineno)
        prb = inliner.problematic(rawtext, rawtext, msg)
        return [prb], [msg]
    (file, _sep, _section) = path.partition('#')  # remove eventual section
    if os.path.isdir('_build/api/odoc/_html/') and not os.path.isfile(
        '_build/api/odoc/_html/' + file
    ):
        # odoc was run but did not generate the page => path is wrong
        # (this is probably a user error)
        error = 'package_role: no API  ' + path
        msg = inliner.reporter.error(format(error), line=lineno)
        prb = inliner.problematic(rawtext, rawtext, msg)
        return [prb], [msg]
    url = url_prefix + "api/odoc/_html/" + path
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []


def opam_role(_name, rawtext, text, _lineno, _inliner, options={}, _content=[]):
    """Implement the opam role."""
    (text, lib) = parse_role(text)
    tagged = re.match('([^.]+)[.].*', lib)
    if tagged:
        url = "https://opam.ocaml.org/packages/" + tagged.group(1) + "/" + lib
    else:
        url = "https://opam.ocaml.org/packages/" + lib
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []


def src_role(_name, rawtext, text, lineno, inliner, options={}, _content=[]):
    """Implement the src role."""
    (text, src) = parse_role(text)

    # raise a warning if the file does not exist
    file = src
    if not Path(TEZOS_HOME, file).exists():
        msg = [
            inliner.reporter.warning(
                f'source file {file} does not exist in the repo', line=lineno
            )
        ]
    else:
        msg = []

    branch = os.environ.get('CI_COMMIT_REF_NAME', 'master')
    project_url = os.environ.get(
        'CI_MERGE_REQUEST_SOURCE_PROJECT_URL',
        os.environ.get('CI_PROJECT_URL', 'https://gitlab.com/tezos/tezos'),
    )
    if Path(TEZOS_HOME, file).is_file():
        url = project_url + "/-/blob/" + branch + "/" + src
    else:  # most likely a directory
        url = project_url + "/-/tree/" + branch + "/" + src
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], msg


def default_role(
    _name, rawtext, _text, lineno, inliner, options={}, _content=[]
):
    """Implement the default role. This role warns on the usage of single
    backticks and points to preferred alternatives."""

    node = nodes.Text(rawtext, **options)

    file = inliner.document.current_source

    # TODO tezos/tezos#2170: suppress the following check when protocol O
    # becomes active, because at that point, the pages for all active protocols
    # will have been fixed automatically by snapshotting Alpha.

    # skip pages of protocols other than Alpha, to avoid manual backporting
    if re.search("/(active|lima|mumbai|nairobi)/", file):
        return [node], []

    # skip automatically generated pages
    if re.match(".*[.]txt$", file):
        return [node], []

    # raise a warning
    msg = [
        inliner.reporter.warning(
            (
                f'undefined syntax: {rawtext}, '
                + 'use ``...``, *...*, **...**, "...", :math:`...`, etc.'
            ),
            line=lineno,
        )
    ]

    return [node], msg

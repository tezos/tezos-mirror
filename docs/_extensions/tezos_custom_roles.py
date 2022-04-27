# pylint: disable=dangerous-default-value
# pylint: disable=global-statement

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
    app.add_role('package', package_role)
    app.add_role('package-name', package_role)
    app.add_role('package-src', package_role)
    app.add_role('opam', opam_role)
    app.add_role('src', src_role)
    app.add_role('gl', gitlab_role)
    global OPAM_CACHE
    if OPAM_CACHE is not None:
        raise ValueError('package_role: opam cache already set!!!')
    OPAM_CACHE = {}
    for path, _, files in os.walk('..'):
        for file in files:
            parts = re.match("^([^/]*)[.]opam$", file)
            if parts:
                name = parts.group(1)
                OPAM_CACHE[name] = path.lstrip('../')
    print("package_role: cached", len(OPAM_CACHE), "opam packages")
    return {'parallel_read_safe': True}


def find_dot_opam(name):
    if name in OPAM_CACHE:
        return OPAM_CACHE[name]
    raise ValueError('opam file ' + name + '.opam does not exist in the odoc')


def parse_role(text: str) -> Tuple[str, str]:
    parts = re.match(r"^([^<>]*)<([^<>]*)>$", text)
    if parts:
        description = parts.group(1)
        obj = parts.group(2)
    else:
        description = text
        obj = text
    return (description, obj)


def package_role(
    name, rawtext, text, _lineno, inliner, options={}, _content=[]
):
    rel_lvl = inliner.document.current_source.replace(os.getcwd(), '').count(
        '/'
    )
    (text, lib) = parse_role(text)
    src = find_dot_opam(lib)
    branch = os.environ.get('CI_COMMIT_REF_NAME', 'master')
    project_url = os.environ.get(
        'CI_MERGE_REQUEST_SOURCE_PROJECT_URL',
        os.environ.get('CI_PROJECT_URL', 'https://gitlab.com/tezos/tezos'),
    )
    src_url = project_url + "/tree/" + branch + "/" + src
    if os.path.isdir('_build/api/odoc/_html/' + lib):
        if os.path.isdir(
            os.path.join(
                '_build',
                'api',
                'odoc',
                '_html',
                lib,
                lib.replace('-', '_').capitalize(),
            )
        ):
            lib = lib + '/' + lib.replace('-', '_').capitalize()
        url = "api/odoc/_html/" + lib + '/index.html'
        for _ in range(1, rel_lvl):
            url = '../' + url
    else:
        url = src_url
    if name == 'package':
        node = nodes.reference(rawtext, text, refuri=url, **options)
    elif name == 'package-name':
        node = nodes.literal(text, text)
    elif name == 'package-src':
        node = nodes.reference(rawtext, src, refuri=src_url, **options)
    return [node], []


def opam_role(_name, rawtext, text, _lineno, inliner, options={}, _content=[]):
    _rel_lvl = inliner.document.current_source.replace(os.getcwd(), '').count(
        '/'
    )
    (text, lib) = parse_role(text)
    tagged = re.match('([^.]+)[.].*', lib)
    if tagged:
        url = "https://opam.ocaml.org/packages/" + tagged.group(1) + "/" + lib
    else:
        url = "https://opam.ocaml.org/packages/" + lib
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []


def src_role(_name, rawtext, text, lineno, inliner, options={}, _content=[]):
    _rel_lvl = inliner.document.current_source.replace(os.getcwd(), '').count(
        '/'
    )
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

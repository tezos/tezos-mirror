"""Defines a special role, :gl:, for referring to GitLab objects (merge
requests, issues, etc)."""

# pylint: disable=dangerous-default-value

import re

from docutils import nodes

GL_URL = "https://gitlab.com/"


def expand_gitlab_shortlink(
    text, default_namespace="tezos", default_project="tezos"
):
    """Expands Gitlab Special References, i.e. tezos/tezos!99 to URLs."""

    def re_expander(pattern):
        return lambda match: re.sub(
            r"\\(\d+)", lambda m2: match.group(int(m2.group(1))), pattern
        )

    def std_expander(match):
        obj_types = {
            "#": "issues",
            "!": "merge_requests",
            "$": "snippets",
            "%": "milestones",
        }
        namespace = match.group("namespace") or default_namespace
        project = match.group("project") or default_project
        obj_typ = obj_types[match.group("type")]
        obj = match.group("obj")
        return f"{namespace}/{project}/-/{obj_typ}/{obj}"

    def commit_expander(match):
        namespace = match.group("namespace") or default_namespace
        project = match.group("project") or default_project
        obj_typ = "commit" if match.group("commit_end") is None else "compare"
        obj = match.group("obj")
        return f"{namespace}/{project}/-/{obj_typ}/{obj}"

    def const_expander(cst):
        return lambda _: cst

    short_link_formats = [
        (r"@(?!all)([a-zA-Z0-9_-]+)$", re_expander("\\1")),
        (r"@all$", const_expander(f"{default_namespace}/{default_project}")),
        (r"([^@/]+)/([^/!#$%]+)>$", re_expander("\\1/\\2")),
        (
            r"((?P<namespace>[^/!#$%]+)/)?"
            + r"(?P<project>[^/!#$%]+)?"
            + r"(?P<type>[!#$%])"
            + r"(?P<obj>.*)",
            std_expander,
        ),
        (
            r"((?P<namespace>[^/!#$%]+)/)?"
            + r"((?P<project>[^/!#$%]+)@)?"
            + r"(?P<obj>[a-f0-9]{8}(...(?P<commit_end>[a-f0-9]{8}))?)$",
            commit_expander,
        ),
    ]

    expansions = []
    for (regexp, expander) in short_link_formats:
        if match := re.match(regexp, text):
            expansion = expander(match)
            expansions.append(GL_URL + expansion)

    return expansions


def gitlab_role(_name, rawtext, text, lineno, inliner, options={}, _content=[]):
    """Implements the :gl: GitLab special reference role"""
    parts = re.match("^([^<>]*)<([^<]*)>$", text)
    if parts:
        text = parts.group(1)
        ref = parts.group(2)
    else:
        ref = text

    expansions = expand_gitlab_shortlink(ref)
    if len(expansions) == 0:
        raise ValueError(
            f"could not resolve GitLab shortlink `{ref}`: invalid format?"
        )

    if len(expansions) > 1:
        msg = [
            inliner.reporter.error(
                f"ambiguous GitLab shortlink `{ref}`, could expand to any of: "
                + f'{", ".join(expansions)}, picking the first',
                line=lineno,
            )
        ]
    else:
        msg = []

    url = expansions[0]
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], msg

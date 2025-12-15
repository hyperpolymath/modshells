---
name: Documentation
about: Report unclear, missing, or incorrect documentation
title: "[DOCS]: "
labels: 'documentation, priority: unset, triage'
assignees: ''

---

name: Documentation
description: Report unclear, missing, or incorrect documentation
title: "[Docs]: "
labels: ["documentation", "triage"]
body:
  - type: markdown
    attributes:
      value: |
        Help us improve our documentation by reporting issues or gaps.

  - type: dropdown
    id: type
    attributes:
      label: Documentation issue type
      options:
        - Missing (documentation doesn't exist)
        - Incorrect (information is wrong)
        - Unclear (confusing or hard to follow)
        - Outdated (no longer accurate)
        - Typo or grammar
    validations:
      required: true

  - type: input
    id: location
    attributes:
      label: Location
      description: Where is this documentation? (URL, file path, or section name)
      placeholder: README.adoc, section "Installation"
    validations:
      required: true

  - type: textarea
    id: description
    attributes:
      label: Description
      description: What's the problem with the current documentation?
      placeholder: Describe what's wrong or missing
    validations:
      required: true

  - type: textarea
    id: suggestion
    attributes:
      label: Suggested improvement
      description: How should it be fixed or improved?
      placeholder: The documentation should say...
    validations:
      required: false

  - type: checkboxes
    id: contribution
    attributes:
      label: Contribution
      options:
        - label: I would be willing to submit a PR to fix this
          required: false

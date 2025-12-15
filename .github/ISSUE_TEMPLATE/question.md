---
name: Question
about: Ask a question about usage or behaviour
title: "[QUESTION]: "
labels: question, triage
assignees: ''

---

name: Question
description: Ask a question about usage or behaviour
title: "[Question]: "
labels: ["question", "triage"]
body:
  - type: markdown
    attributes:
      value: |
        Have a question? You can also ask in [Discussions](../discussions) for broader conversations.

  - type: textarea
    id: question
    attributes:
      label: Your question
      description: What would you like to know?
      placeholder: How do I...?
    validations:
      required: true

  - type: textarea
    id: context
    attributes:
      label: Context
      description: Any relevant context that helps us answer your question
      placeholder: I'm trying to achieve X and I've tried Y...
    validations:
      required: false

  - type: textarea
    id: research
    attributes:
      label: What I've already tried
      description: What have you already looked at or attempted?
      placeholder: I've read the README and searched issues but...
    validations:
      required: false

  - type: checkboxes
    id: checked
    attributes:
      label: Pre-submission checklist
      options:
        - label: I have searched existing issues and discussions
          required: true
        - label: I have read the documentation
          required: true

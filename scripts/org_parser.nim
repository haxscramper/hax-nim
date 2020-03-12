import options

type
  OrgTimestamp = object
    nil

  OrgSection = object
    nil

  OrgHeadline = object
    sections: seq[OrgSection]

    isArchived: bool
    isCommented: bool
    isFootnote: bool
    isQuoted: bool


    level: int
    preblank: int
    priority: char

    closed: Option[OrgTimestamp]
    deadline: Option[OrgTimestamp]
    scheduled: Optino[OrgTimestamp]
    rawHeadline: string
    tags: seq[string]
    todoKeyword: string
    isTodoKwd: bool

    # :title
    # Parsed headline's text, without the stars and the tags (secondary string).

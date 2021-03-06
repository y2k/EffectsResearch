module Types

open System

type Source =
    | FeedSource
    | TagSource of string
    | FavoriteSource

type Tag =
    { name: string
      image: string }

type Attachment =
    { url: string
      aspect: float }

type AttachmentResource =
    { image: Attachment }

type Comment =
    { text: string
      image: Attachment
      rating: float
      userName: string
      attachments: AttachmentResource [] }

type Post =
    { id: int
      userName: string
      userImage: Attachment
      rating: double
      created: DateTime
      image: Attachment option
      attachments: AttachmentResource []
      title: string
      tags: string []
      comments: Comment [] }

type PostResponse =
    { posts: Post list
      nextPage: int option }

type PostsWithLevels =
    { actual: Post []
      old: Post []
      preloaded: Post []
      nextPage: int option }
    static member empty: PostsWithLevels =
        { actual = [||]
          old = [||]
          preloaded = [||]
          nextPage = None }

type Profile =
    { userName: string
      userImage: Attachment
      rating: float
      stars: int
      progressToNewStar: float }

type Message =
    { text: String
      date: Double
      isMine: Boolean
      userName: String
      userImage: String }

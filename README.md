# prisma-generator-rel8

> This generator was bootstraped using [create-prisma-generator](https://github.com/YassinEldeeb/create-prisma-generator)

# Example

`schema.prisma`

```
generator custom_generator {
  provider = "node ../../node_modules/prisma-rel8-generator"
  output   = "../types"
}

datasource db {
  provider = "sqlite"
  url      = "file:./dev.db"
}

model Author {
  @@map("authors")

  author_id    Int     @id @default(autoincrement())
  name String  @unique
  url  String?
  projects Project[]
}

model Project {
  @@map("projects")
  project_id Int @id @default(autoincrement())
  author_id Int
  author   Author @relation(fields: [author_id], references: [author_id])

  name String
}
```

Generated `models.hs`

```

module Database.Models where

import Internal.Prelude
import Rel8

newtype UserModelId = UserModelId { toInt64 :: Int64 }
    deriving newtype (DBEq, DBType, Eq, Show)
      

data UserModel f = UserModel
    { userModelId :: Column f UserModelId
     ,userModelEmail:: Column f Text
     ,userModelPassword:: Column f Text
     ,userModelName:: Column f (Maybe Text)
     ,userModelCreatedAt:: Column f UTCTime
     ,userModelUpdatedAt:: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

userModelSchema :: TableSchema (UserModel Name)
userModelSchema = TableSchema
  { name = "users"
  , schema = Nothing
  , columns = UserModel
      { userModelId = "id"
       ,userModelEmail = "email"
       ,userModelPassword = "password"
       ,userModelName = "name"
       ,userModelCreatedAt = "created_at"
       ,userModelUpdatedAt = "updated_at"
      }
  }
      

deriving stock instance f ~ Result => Show (UserModel f)

newtype ProjectModelId = ProjectModelId { toInt64 :: Int64 }
    deriving newtype (DBEq, DBType, Eq, Show)
      

data ProjectModel f = ProjectModel
    { projectModelId :: Column f ProjectModelId
     ,projectModelName:: Column f Text
     ,projectModelUserId:: Column f UserModelId
     ,projectModelCreatedAt:: Column f UTCTime
     ,projectModelUpdatedAt:: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

projectModelSchema :: TableSchema (ProjectModel Name)
projectModelSchema = TableSchema
  { name = "projects"
  , schema = Nothing
  , columns = ProjectModel
      { projectModelId = "id"
       ,projectModelName = "name"
       ,projectModelUserId = "user_id"
       ,projectModelCreatedAt = "created_at"
       ,projectModelUpdatedAt = "updated_at"
      }
  }
      

deriving stock instance f ~ Result => Show (ProjectModel f)
    
```

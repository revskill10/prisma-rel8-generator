
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
    
object Persistence: TPersistence
  OldCreateOrder = False
  Height = 150
  Width = 370
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=../../../../DB/DataBase.sqlite3'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 32
    Top = 56
  end
  object FDQuery: TFDQuery
    CachedUpdates = True
    Connection = FDConnection
    UpdateObject = FDUpdateSQL
    SQL.Strings = (
      'SELECT * FROM persons')
    Left = 112
    Top = 56
  end
  object FDUpdateSQL: TFDUpdateSQL
    Connection = FDConnection
    InsertSQL.Strings = (
      'INSERT INTO PERSONS'
      '(NAME)'
      'VALUES (:NEW_NAME);'
      'SELECT LAST_INSERT_AUTOGEN() AS ID')
    ModifySQL.Strings = (
      'UPDATE PERSONS'
      'SET ID = :NEW_ID, NAME = :NEW_NAME'
      'WHERE ID = :OLD_ID;'
      'SELECT ID'
      'FROM PERSONS'
      'WHERE ID = :NEW_ID')
    DeleteSQL.Strings = (
      'DELETE FROM PERSONS'
      'WHERE ID = :OLD_ID')
    FetchRowSQL.Strings = (
      'SELECT LAST_INSERT_AUTOGEN() AS ID, NAME'
      'FROM PERSONS'
      'WHERE ID = :ID')
    Left = 192
    Top = 56
  end
  object FDStanStorageBinLink: TFDStanStorageBinLink
    Left = 288
    Top = 56
  end
end

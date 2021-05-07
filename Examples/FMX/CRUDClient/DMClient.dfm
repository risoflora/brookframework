object Client: TClient
  OldCreateOrder = False
  Height = 150
  Width = 303
  object FDMemTable: TFDMemTable
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 40
    Top = 48
  end
  object NetHTTPClient: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    AllowCookies = True
    HandleRedirects = False
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 232
    Top = 48
  end
  object FDStanStorageBinLink: TFDStanStorageBinLink
    Left = 136
    Top = 48
  end
end

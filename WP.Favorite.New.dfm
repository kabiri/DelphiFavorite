object frmNewFavorite: TfrmNewFavorite
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Favorite'
  ClientHeight = 415
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object lblFavoriteName: TLabel
    Left = 9
    Top = 11
    Width = 75
    Height = 15
    Caption = 'Favorite name'
  end
  object Label1: TLabel
    Left = 9
    Top = 43
    Width = 89
    Height = 15
    Caption = 'Selected Projects'
  end
  object Label2: TLabel
    Left = 414
    Top = 43
    Width = 93
    Height = 15
    Caption = 'Available Projects'
  end
  object edtFavoriteName: TEdit
    Left = 90
    Top = 8
    Width = 251
    Height = 23
    TabOrder = 0
  end
  object btnSave: TButton
    Left = 6
    Top = 366
    Width = 92
    Height = 41
    Caption = 'Save'
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 118
    Top = 366
    Width = 92
    Height = 41
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnAdd: TButton
    Left = 366
    Top = 170
    Width = 33
    Height = 33
    Caption = '<'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnDel: TButton
    Left = 366
    Top = 209
    Width = 33
    Height = 33
    Caption = '>'
    TabOrder = 4
    OnClick = btnDelClick
  end
  object pnlAvailable: TPanel
    AlignWithMargins = True
    Left = 414
    Top = 64
    Width = 346
    Height = 289
    BevelInner = bvLowered
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 5
    object sbAvailable: TScrollBox
      Left = 2
      Top = 2
      Width = 342
      Height = 285
      VertScrollBar.Tracking = True
      Align = alClient
      TabOrder = 0
    end
  end
  object pnlSelected: TPanel
    AlignWithMargins = True
    Left = 6
    Top = 64
    Width = 346
    Height = 287
    BevelInner = bvLowered
    Caption = 'pnlSelected'
    ShowCaption = False
    TabOrder = 6
    object sbSelected: TScrollBox
      Left = 2
      Top = 2
      Width = 342
      Height = 283
      VertScrollBar.Tracking = True
      Align = alClient
      TabOrder = 0
    end
  end
end

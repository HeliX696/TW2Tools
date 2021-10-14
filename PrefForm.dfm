object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Options'
  ClientHeight = 156
  ClientWidth = 274
  Color = 16244682
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 105
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 26
      Width = 134
      Height = 16
      Caption = 'Minimal column size: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit1: TEdit
      Left = 144
      Top = 24
      Width = 121
      Height = 19
      TabOrder = 0
      Text = '30'
      OnKeyPress = Edit1KeyPress
    end
  end
  object Button1: TButton
    Left = 185
    Top = 111
    Width = 81
    Height = 34
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 111
    Width = 82
    Height = 34
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
  end
end

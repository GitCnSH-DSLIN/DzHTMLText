unit UFrmExample;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, DzHTMLText, Vcl.Graphics,
  Vcl.ImgList, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    MyImages: TImageList;
    btn1: TButton;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer;
      LinkData: TDHLinkData; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    Lb: TDzHTMLText;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Vcl.Dialogs, System.SysUtils;

procedure TForm1.btn1Click(Sender: TObject);
begin
  Lb.Rebuild;
  Self.Caption := Format('W=%d, H=%d, TW=%d, TH=%d', [Lb.Width, Lb.Height, lb.TextWidth, lb.TextHeight]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lb := TDzHTMLText.Create(Self);
  Lb.Parent := Self;
  // Lb.Align := alClient;
  with Lb do begin
    //Visible := False;
    BeginUpdate;
    Left := 8;
    Top := 8;
    Width := 313;
    Height := 391;
    Font.Color := clWindowText;
    Font.Height := -13;
    Font.Name := 'Segoe UI';
    Font.Style := [];
    ParentFont := False;
    Text := 'Welcome to my app!<br><br><b>This is an important text很好,亲给我衣蛾测试的额理由的jsidiot库回复</b> and <'
      + 'i>this is italic text</i>.<br><u>But we have underline too</u>, ' +
      'and <s>strike-out</s> if you want.<br><br>You can change the <fn' +
      ':Courier New>font name</fn> and the <fs:14>font size</fs><br>The' +
      ' <fc:clBlue>colors</fc> are <bc:clYellow>allowed</bc> too! <img:' +
      '0> <i>and images</i> <img:1><br><br><c>Alignment, we have!</c><b' +
      'r><r>This is great</r><br><br>You can use tab align too:'#13#10'1<t:30' +
      '>大张伟<t:100>100.000'#13#10'2<t:30>社辣菜<t:100>150.000'#13#10'3<t:30>ERIC<t:1'
      + '00>180.000'#13#10#13#10'Click <a:www.google.com.br>here to open Google</a>'
      + '.'#13#10'Click <a:MSG_BOX>显示消息对话框</a>.';
    AutoWidth := True;
    AutoHeight := True;
    Images := MyImages;
    OnLinkClick := LbLinkClick;
    LineVertAlign := vaCenter;
    EndUpdate(False);
  end;
end;

procedure TForm1.LbLinkClick(Sender: TObject; LinkID: Integer;
  LinkData: TDHLinkData; var Handled: Boolean);
begin
  if LinkData.Target = 'MSG_BOX' then begin
    ShowMessage('You have clicked at message box link!');
    Handled := True;
  end;
end;

end.

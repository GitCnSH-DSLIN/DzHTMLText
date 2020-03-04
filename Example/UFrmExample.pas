unit UFrmExample;

interface

uses Winapi.Windows, Vcl.Forms, System.Classes, Vcl.Controls, DzHTMLText, Vcl.Graphics,
  Vcl.ImgList, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    MyImages: TImageList;
    btn1: TButton;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer;
      LinkData: TDHLinkData; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    Lb: TDzHTMLText;

    procedure OnHtmlRepaint(Sender: TObject; ARect: TRect);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Vcl.Dialogs, System.SysUtils;

procedure TForm1.btn1Click(Sender: TObject);
begin
  //Lb.Rebuild;

  Lb.PaintTo(Self.Canvas, Lb.Location.Left, Lb.Location.Top);
  //Self.Caption := Format('W=%d, H=%d, TW=%d, TH=%d', [Lb.Width, Lb.Height, lb.TextWidth, lb.TextHeight]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lb := TDzHTMLText.Create;

  // Lb.Align := alClient;
  with Lb do begin
    BeginUpdate;
    Location := Rect(50, 50, 340,100);
    BGColor := clBtnFace;
    Text := 'Welcome to my app!<br><br><b>This is an important text很好,亲给我' +
      '衣蛾测试的额理由的jsidiot库回复</b> and <' +
      'i>this is italic text</i>.<br><u>But we have underline too</u>, ' +
      'and <s>strike-out</s> if you want.<br><br>You can change the <fn' +
      ':Courier New>font name</fn> and the <fs:14>font size</fs><br>The' +
      ' <fc:clBlue>colors</fc> are <bc:clYellow>allowed</bc> too! <img:' +
      '0> <i>and images</i> <a:MSG_BOX>不错<img:1>YES</a><br><br><c>Alignment, we have!</c><b' +
      'r>免费都不爱<r>This is great YES you CAM Please Give You asn Mingtubu Lin' +
      ' DeshengHhai合法霸道电动车的成熟度</r><br><br>You can use tab align too:'#13#10'1<t:30' +
      '>大张伟<t:100>100.000'#13#10'2<t:30>社辣菜<t:100>150.000'#13#10'3<t:30>ERIC<t:1' +
      '00>180.000'#13#10#13#10'Click <a:www.google.com.br>here to open Google</a>' +
      '.'#13#10'Click <a:MSG_BOX>显示消息对话框</a>.';
    //Text := 'text很好,亲给我衣蛾测试的额理由的jsidiot库回复显示消息对话框';
    //MaxWidth := 300;
    //AutoWidth := True;
    AutoHeight := True;
    Images := MyImages;
    OnLinkClick := LbLinkClick;
    LineVertAlign := vaCenter;
    OnRepaint := OnHtmlRepaint;
    EndUpdate(True);
  end;

  //Lb.Location.Width := ;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Cursor: Integer;
begin
  //Self.Caption := Format('X=%d, Y=%d', [X, Y]);

  Cursor := crDefault;
  Lb.MouseMove(Sender, Shift, X - Lb.Location.Left, Y - Lb.Location.Top, Cursor);
  if Cursor <> Self.Cursor then
    Self.Cursor := Cursor;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Lb.MouseUp(Sender, Button, Shift, X - Lb.Location.Left, Y - Lb.Location.Top);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  r: trect;
begin
  r := Self.Canvas.ClipRect;
  Self.Caption := Format('L=%d, T=%d, W=%d, H=%d', [r.Left, r.Top, r.Width, r.Height]);
  if IntersectRect(r, r, Lb.Location) then begin
    OffsetRect(r, -Lb.Location.Left, -Lb.Location.Top);
    Lb.PaintRectTo(Self.Canvas, Self.Canvas.ClipRect.Left, Self.Canvas.ClipRect.Top, r);
  end;

  Self.Canvas.Brush.Style := bsClear;
  Self.Canvas.Rectangle(Lb.Location);
end;

procedure TForm1.LbLinkClick(Sender: TObject; LinkID: Integer;
  LinkData: TDHLinkData; var Handled: Boolean);
begin
  if LinkData.Target = 'MSG_BOX' then begin
    ShowMessage('You have clicked at message box link!');
    Handled := True;
  end;
end;

procedure TForm1.OnHtmlRepaint(Sender: TObject; ARect: TRect);
begin
  Invalidaterect(Self.Handle, ARect, False);
end;

end.

using System.Diagnostics;

namespace ConsoleUI
{
    public enum FocusDirection
    {
        Horizontal,
        Vertical,
    }

    /** Håndterer en samling widgets som kan tegnes på skærmen og håndterer interaktioner. */
    public class Screen
    {
        private ITerminal _terminal;
        private DrawManager _drawManager;
        private FocusManager _focusManager;

        public Widget[] Elements
        {
            get => _drawManager.Elements;
            set
            {
                _drawManager.Elements = value;
                _focusManager.Elements = Elements.OfType<Focusable>().ToArray();
            }
        }

        public Screen(ITerminal terminal)
        {
            _drawManager = new DrawManager(terminal);
            _focusManager = new FocusManager(Array.Empty<Focusable>());
            _terminal = terminal;
        }

        public FocusDirection FocusDirection;

        public void SetFocusOrder(FocusDirection direction, Focusable[]? elements = null)
        {
            FocusDirection = direction;
            if (elements is not Focusable[])
                elements = Elements.OfType<Focusable>().ToArray();

            _focusManager.Elements = elements;
        }

        public void FocusElement(Focusable element)
        {
            _focusManager.SetFocused(element);
        }

        public Task Draw()
        {
            return new Task(_drawManager.DrawLoop);
        }

        public void RefreshScreen() => _drawManager.RefreshNow();

        public async Task<bool> HandleInput()
        {
            var key = await _terminal.ReadKey(true);
            if (!_focusManager.OnKey(key))
            {
                if (key.Key is ConsoleKey.Tab) _focusManager.ShiftFocus();
                else if (FocusDirection is FocusDirection.Horizontal)
                {
                    if (key.Key is ConsoleKey.LeftArrow) _focusManager.ShiftFocus(-1);
                    else if (key.Key is ConsoleKey.RightArrow) _focusManager.ShiftFocus(1);
                    else return false;
                    return true;
                }
                else if (FocusDirection is FocusDirection.Vertical)
                {
                    if (key.Key is ConsoleKey.UpArrow) _focusManager.ShiftFocus(-1);
                    else if (key.Key is ConsoleKey.DownArrow) _focusManager.ShiftFocus(1);
                    else return false;
                    return true;
                }
                else return false;
            }

            return true;
        }

        public void StopDrawing()
        {
            _drawManager.Exit = true;
            _drawManager.RefreshNow();
        }
    }


    class DrawManager
    {
        private Widget[] _elements = { };
        private Refreshable[] _refreshableElements = { };
        private CancellationTokenSource _wait = new();
        public bool Exit = false;
        private Stopwatch _stopwatch = new();
        private ITerminal _terminal;

        public DrawManager(ITerminal terminal)
        {
            _terminal = terminal;
        }

        public Widget[] Elements
        {
            get => _elements;
            set
            {
                _elements = value;
                _refreshableElements = value.OfType<Refreshable>().ToArray();
            }
        }

        public void RefreshNow()
        {
            _wait.Cancel();
        }

        public async void DrawLoop()
        {
            long? nextRefresh = null;

            while (!Exit)
            {
                _terminal.Clear();
                var constraints = new Vec2(_terminal.WindowWidth, _terminal.WindowHeight);
                foreach (var element in _elements)
                {
                    var measured = element.Measure(constraints);
                    foreach (var drawable in element.Draw(measured))
                        drawable.Draw(Vec2.Zero, _terminal);
                }
                _terminal.Flush();

                if (nextRefresh is null)
                {
                    var timeout = _refreshableElements.Min(el => el.RefreshInMillis());
                    if (timeout is long wait)
                    {
                        var currentTime = _stopwatch.ElapsedMilliseconds;
                        nextRefresh = currentTime + wait;
                    }
                }

                try
                {
                    if (nextRefresh is long next)
                    {
                        var currentTime = _stopwatch.ElapsedMilliseconds;
                        var timeout = next - currentTime;
                        await Task.Delay((int)timeout, _wait.Token);

                        nextRefresh = null;
                    }
                    else await Task.Delay(-1, _wait.Token);
                }
                catch (TaskCanceledException e)
                {
                    // rethrow if _wait was not the cancellation source
                    if (e.CancellationToken != _wait.Token) throw;
                    _wait = new CancellationTokenSource();
                }
            }
        }
    }

    public interface Refreshable : Widget
    {
        /** Returnerer om hvor lang tid dette element skal opdateres eller null hvis ikke */
        public long? RefreshInMillis();
    }

    public interface Drawable
    {
        public void Draw(Vec2 offset, ITerminal terminal);
    }

    public struct DrawCall : Drawable
    {
        public Vec2 Position;

        /**
         * 0: Tekst vil blive ankeret i venstre side
         * 0.5: Tekst vil blive centeret
         * 1: Takst vil blive ankeret i højre side
         */
        public double HorizontalAlignment;

        public string Output;
        public ConsoleColor? BackgroundColor;
        public ConsoleColor? ForegroundColor;

        public DrawCall(string output) : this()
        {
            Output = output;
        }

        public void Draw(Vec2 offset, ITerminal terminal)
        {
            var x = (int)(Position.X + offset.X);
            var y = (int)(Position.Y + offset.Y);

            if (BackgroundColor != null)
                terminal.BackgroundColor = (ConsoleColor)BackgroundColor;
            if (ForegroundColor != null)
                terminal.ForegroundColor = (ConsoleColor)ForegroundColor;

            var lines = Output.Split('\n');
            for (var i = 0; i < lines.Length; i++)
            {
                var cursorY = y + i;
                // undgå at skrive uden for vinduets rammer
                if (cursorY < 0 || cursorY >= terminal.WindowHeight) continue;

                terminal.SetCursorPosition(
                    (int)double.Round(x - lines[i].Length * HorizontalAlignment),
                    cursorY
                );
                terminal.Write(lines[i]);
            }

            terminal.ResetColor();
        }
    }

    public struct Frame : Drawable
    {
        public Vec2 Position;
        public List<Drawable> Children;

        public void Draw(Vec2 offset, ITerminal terminal)
        {
            var position = Position + offset;
            foreach (var drawable in Children)
                drawable.Draw(position, terminal);
        }
    }

    public interface Widget
    {
        Vec2 Measure(Vec2 constraints);

        List<Drawable> Draw(Vec2 measured);
    }

    public interface Container : Refreshable
    {
        Rect? BoundsOfChild(Widget child);
    }

    public interface Focusable : Widget
    {
        bool Focused { get; set; }

        bool OnKey(ConsoleKeyInfo key);

        Vec2 Hotspot(Vec2 measured);
    }

    public struct Vec2
    {
        public static Vec2 Zero = new(0.0, 0.0);
        public static Vec2 One = new(1.0, 1.0);
        public static Vec2 Right = new(1.0, 0.0);
        public static Vec2 Down = new(0.0, 1.0);

        public double X, Y;

        public Vec2(double x, double y)
        {
            X = x;
            Y = y;
        }

        public static Vec2 operator +(Vec2 a, Vec2 b) => new(a.X + b.X, a.Y + b.Y);
        public static Vec2 operator -(Vec2 a, Vec2 b) => new(a.X - b.X, a.Y - b.Y);
        public static Vec2 operator *(Vec2 a, Vec2 b) => new(a.X * b.X, a.Y * b.Y);
        public static Vec2 operator *(Vec2 a, double scalar) => new(a.X * scalar, a.Y * scalar);
    }

    public struct Rect
    {
        public Vec2 From, To;

        public Rect(Vec2 from, Vec2 to)
        {
            From = from;
            To = to;
        }

        public static Rect FromSize(Vec2 position, Vec2 size) => new(position, position + size);

        public Vec2 Size => new(To.X - From.X, To.Y - From.Y);

        public Rect Expand(Vec2 amount) => new(From, To + amount);

        public static Rect operator +(Rect a, Vec2 b) => new(a.From + b, a.To + b);
    }

    public class LabelWidget : Widget
    {
        public string Label;
        private double _horizontalAlignment;
        public int MinWidth;

        public LabelWidget(string label, double horizontalAlignment)
        {
            Label = label;
            _horizontalAlignment = horizontalAlignment;
        }

        int LongestLine() => Math.Max(Label.Split('\n').Max(line => line.Length), MinWidth);

        int LineCount() => Label.Split('\n').Length;

        public Vec2 Measure(Vec2 constraints)
        {
            var maxWidth = LongestLine();
            var maxHeight = LineCount();

            return new Vec2
            {
                X = Math.Min(constraints.X, maxWidth),
                Y = Math.Min(constraints.Y, maxHeight),
            };
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            var output = string.Join('\n', Label.Split('\n').Select(line =>
            {
                if (line.Length > measured.X)
                    return line[..((int)measured.X - 3)] + "...";
                return line;
            }));

            return new List<Drawable>
            {
                new DrawCall
                {
                    Output = output,
                    HorizontalAlignment = _horizontalAlignment,
                    Position = new Vec2(measured.X * _horizontalAlignment, 0)
                }
            };
        }
    }

    public class InputFieldWidget : Widget, Focusable
    {
        public Predicate<char> CharPredicate;
        public int MaxWidth;
        public string Contents;
        public double HorizontalAlignment;
        public bool Focused { get; set; }

        public InputFieldWidget(Predicate<char> predicate, int maxWidth = 20, string contents = "")
        {
            CharPredicate = predicate;
            MaxWidth = maxWidth;
            Contents = contents;
        }

        public Vec2 Measure(Vec2 constraints) => new(Math.Min(MaxWidth, constraints.X), 1);

        public List<Drawable> Draw(Vec2 measured)
        {
            return new List<Drawable>
            {
                new DrawCall
                {
                    Position = new Vec2((measured.X - Contents.Length) * HorizontalAlignment, 0),
                    Output = Contents
                },
                new DrawCall
                {
                    Position = new Vec2(0, 1),
                    Output = new string(Focused ? '\u2594' : '‾', (int)measured.X),
                    ForegroundColor = Focused ? ConsoleColor.White : ConsoleColor.Gray
                }
            };
        }

        public Vec2 Hotspot(Vec2 measured)
        {
            return new Vec2((measured.X - Contents.Length) * HorizontalAlignment + Contents.Length, 0);
        }

        public bool OnKey(ConsoleKeyInfo key)
        {
            if (key.Key is ConsoleKey.Backspace)
            {
                if (Contents.Length > 0) Contents = Contents[..^1];
                return true;
            }

            if (char.IsControl(key.KeyChar) || !CharPredicate(key.KeyChar)) return false;

            if (Contents.Length < MaxWidth)
                Contents += key.KeyChar;
            return true;
        }
    }

    public struct PlacedWidget
    {
        public Widget Widget;
        public Rect Bounds;
    }

    public struct PlacementState
    {
        public PlacedWidget[] PlacedWidgets;
        public Dictionary<Widget, int> Lookup;
    }

    public class LayoutWidget : Widget, Container
    {
        private Widget[] _elements;
        private PlacementState _placementState;
        private Vec2 _stackingAxis;
        private double _alignment;

        public Widget[] Children
        {
            get => _elements;
            set
            {
                _elements = value;
                _placementState.PlacedWidgets = new PlacedWidget[] { };
                _placementState.Lookup = new Dictionary<Widget, int>();
            }
        }

        public LayoutWidget(Widget[] elements, Vec2 stackingAxis, double alignment = 0.5)
        {
            _elements = elements;
            _stackingAxis = stackingAxis;
            _alignment = alignment;
        }

        public Vec2 PositionOf(Widget widget)
        {
            var index = _placementState.Lookup[widget];
            var placement = _placementState.PlacedWidgets[index];
            return placement.Bounds.From;
        }

        public Rect? BoundsOfChild(Widget child)
        {
            foreach (var placedWidget in _placementState.PlacedWidgets)
            {
                if (placedWidget.Widget == child) return placedWidget.Bounds;

                if (placedWidget.Widget is not Container childContainer) continue;

                var nestedPosition = childContainer.BoundsOfChild(child);
                return nestedPosition + placedWidget.Bounds.From;
            }

            return null;
        }

        public Vec2 Measure(Vec2 constraints)
        {
            var accumulator = new Vec2();

            _placementState.PlacedWidgets = new PlacedWidget[_elements.Length];
            _placementState.Lookup = new Dictionary<Widget, int>();

            var remainingSpace = constraints;
            var alignmentAxis = Vec2.One - _stackingAxis;

            for (var i = 0; i < _elements.Length; i++)
            {
                var widget = _elements[i];
                _placementState.Lookup[widget] = i;

                var size = widget.Measure(remainingSpace);
                remainingSpace -= size * _stackingAxis;

                _placementState.PlacedWidgets[i] = new PlacedWidget
                {
                    Widget = widget,
                    Bounds = Rect.FromSize(accumulator, size)
                };

                accumulator += size * _stackingAxis;
            }

            var bounds = _placementState.PlacedWidgets.Aggregate(
                accumulator,
                (max, widget) => new Vec2(Math.Max(max.X, widget.Bounds.Size.X), Math.Max(max.Y, widget.Bounds.Size.Y))
            );

            // apply alignment
            for (var i = 0; i < _placementState.PlacedWidgets.Length; i++)
            {
                _placementState.PlacedWidgets[i].Bounds +=
                    (bounds - _placementState.PlacedWidgets[i].Bounds.Size) * alignmentAxis * _alignment;
            }

            return bounds;
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            return _placementState.PlacedWidgets.Select(widget => new Frame
            {
                Children = widget.Widget.Draw(widget.Bounds.Size),
                Position = widget.Bounds.From,
            } as Drawable).ToList();
        }

        public long? RefreshInMillis() => _elements.OfType<Refreshable>().Min(widget => widget.RefreshInMillis());
    }

    public class FrameWidget : Widget, Container
    {
        public Widget Child;
        public Vec2 Size;
        public Vec2 Alignment;

        private Vec2 _position;
        private Vec2 _childSize;

        public FrameWidget(Widget child, Vec2 size, Vec2 alignment)
        {
            Child = child;
            Size = size;
            Alignment = alignment;
        }

        public Rect? BoundsOfChild(Widget child)
        {
            if (Child == child) return Rect.FromSize(_position, _childSize);
            if (Child is not Container childContainer) return null;

            var nestedPosition = childContainer.BoundsOfChild(child);
            return nestedPosition + _position;
        }

        public Vec2 Measure(Vec2 constraints)
        {
            var frameSize = constraints * Size;
            _childSize = Child.Measure(frameSize);
            _position = (frameSize - _childSize) * Alignment;
            return frameSize;
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            return new List<Drawable>
            {
                new Frame
                {
                    Children = Child.Draw(_childSize),
                    Position = _position
                }
            };
        }

        public long? RefreshInMillis()
        {
            if (Child is Refreshable refreshable)
                return refreshable.RefreshInMillis();
            return null;
        }
    }

    public class SelectorWidget : Focusable
    {
        public string[] Options;
        public int Selection;
        public bool Focused { get; set; }
        public int MaxHeight = 9;

        public SelectorWidget(string[] options)
        {
            Options = options;
        }

        public void MoveSelection(int amount)
        {
            Selection = Math.Clamp(Selection + amount, 0, Options.Length - 1);
        }

        public string SelectedOption => Options[Selection];

        public bool OnKey(ConsoleKeyInfo key)
        {
            if (key.Key == ConsoleKey.DownArrow)
                MoveSelection(1);
            else if (key.Key == ConsoleKey.UpArrow)
                MoveSelection(-1);
            else return false;

            return true;
        }

        public Vec2 Measure(Vec2 constraints)
        {
            return new Vec2
            {
                // +1 til indikator-pile
                X = Math.Min(constraints.X, Options.Max(line => line.Length) + 2),
                Y = Math.Min(MaxHeight, Math.Min(constraints.Y, Options.Length * 2 - 1))
            };
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            var drawCalls = new List<Drawable>();

            if (Focused)
            {
                drawCalls.Add(new DrawCall
                {
                    Output = "⌃",
                    Position = new Vec2(measured.X, measured.Y / 2.0 - 2)
                });
                drawCalls.Add(new DrawCall
                {
                    Output = "⌄",
                    // HorizontalAlignment = 0.5,
                    Position = new Vec2(measured.X, measured.Y / 2.0 + 2)
                });
            }

            var yOffset = measured.Y / 2.0 - Selection;

            for (var i = 0; i < Options.Length; i++)
            {
                var lineY = yOffset + i;
                // spring over linjer som uden for rammen
                if (lineY < 0) continue;
                if (lineY > measured.Y) break;

                var line = Options[i];
                if (line.Length > measured.X)
                    line = line[..((int)measured.X - 4)] + "...";

                var drawCall = new DrawCall
                {
                    Output = line,
                    HorizontalAlignment = 0.5,
                    Position = new Vec2
                    {
                        X = (measured.X - 2) / 2.0,
                        Y = yOffset + i
                    }
                };
                if (Selection == i)
                {
                    drawCall.Output = drawCall.Output.PadLeft(1).PadRight(1);
                    drawCall.BackgroundColor = Focused ? ConsoleColor.White : ConsoleColor.Gray;
                    drawCall.ForegroundColor = ConsoleColor.Black;
                }

                drawCalls.Add(drawCall);
            }

            return drawCalls;
        }

        public Vec2 Hotspot(Vec2 measured) => new(measured.X, measured.Y / 2);
    }

    public class SpaceWidget : Widget
    {
        public Vec2 Space = new(1, 1);

        public SpaceWidget()
        {
        }

        public SpaceWidget(double space)
        {
            Space = new Vec2(space, space);
        }

        public Vec2 Measure(Vec2 constraints)
        {
            return new Vec2(
                Math.Min(Space.X, constraints.X),
                Math.Min(Space.Y, constraints.Y)
            );
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            return new List<Drawable>();
        }
    }

    public class ButtonWidget : Focusable
    {
        public bool Focused { get; set; }
        public string Label = "";
        public bool Padding = true;

        private bool _pressed;

        public bool Pressed
        {
            get
            {
                if (_pressed)
                {
                    _pressed = false;
                    return true;
                }

                return false;
            }
        }

        public Vec2 Measure(Vec2 constraints)
        {
            return new Vec2(
                Math.Min(constraints.X, Label.Length + (Padding ? 4 : 2)),
                Math.Min(constraints.Y, Padding ? 3 : 1)
            );
        }

        public List<Drawable> Draw(Vec2 measured)
        {
            ConsoleColor? foregroundColor = Focused ? ConsoleColor.Black : null;
            ConsoleColor? backgroundColor = Focused ? ConsoleColor.White : null;

            var maxLength = (int)measured.X - (Padding ? 4 : 0);
            var label = Label.Length > maxLength ? Label[..(maxLength - 3)] + "..." : Label;
            var length = (int)measured.X;
            var horizontalPadding = length - label.Length;
            var paddedLabel = new string(' ', horizontalPadding / 2) +
                              label +
                              new string(' ', horizontalPadding / 2 + horizontalPadding % 2);

            var output = "";
            if (Padding) output += new string(' ', length) + '\n';
            output += paddedLabel;
            if (Padding) output += '\n' + new string(' ', length);

            return new List<Drawable>
            {
                new DrawCall
                {
                    Output = output,
                    ForegroundColor = foregroundColor,
                    BackgroundColor = backgroundColor
                }
            };
        }

        public bool OnKey(ConsoleKeyInfo key)
        {
            if (key.Key is not (ConsoleKey.Enter or ConsoleKey.Spacebar)) return false;

            _pressed = true;
            return true;
        }

        public Vec2 Hotspot(Vec2 measured) => Vec2.Zero;
    }

    public class FocusManager
    {
        private Focusable[] _elements;

        public Focusable[] Elements
        {
            get => _elements;
            set
            {
                if (_elements.Length > 0) FocusedElement.Focused = false;
                _elements = value;
                _lookup = LookupFor(_elements);

                if (_elements.Length > 0)
                {
                    _focused = Math.Clamp(_focused, 0, _elements.Length - 1);
                    FocusedElement.Focused = true;
                }
                else _focused = 0;
            }
        }

        private int _focused;
        private Dictionary<Focusable, int> _lookup = new();

        public FocusManager(Focusable[] elements, int initial = 0)
        {
            _elements = elements;
            _focused = Math.Clamp(initial, 0, _elements.Length);
            _lookup = LookupFor(_elements);
            if (_elements.Length > 0) FocusedElement.Focused = true;
        }

        private static Dictionary<Focusable, int> LookupFor(Focusable[] widgets)
        {
            var lookup = new Dictionary<Focusable, int>();
            for (var i = 0; i < widgets.Length; i++)
                lookup[widgets[i]] = i;
            return lookup;
        }

        public Focusable FocusedElement =>
            Elements[_focused];

        public bool OnKey(ConsoleKeyInfo key) => FocusedElement.OnKey(key);

        public void SetFocused(Focusable element)
        {
            FocusedElement.Focused = false;
            element.Focused = true;
            _focused = _lookup[element];
        }

        public void ShiftFocus(int amount = 1)
        {
            FocusedElement.Focused = false;
            _focused = Math.Clamp(_focused + amount, 0, Elements.Length - 1);
            FocusedElement.Focused = true;
        }
    }
}
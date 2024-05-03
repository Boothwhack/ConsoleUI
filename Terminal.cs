namespace ConsoleUI;

public interface ITerminal
{
    int WindowWidth { get; }
    int WindowHeight { get; }

    ConsoleColor BackgroundColor { get; set; }
    ConsoleColor ForegroundColor { get; set; }

    void ResetColor();

    void SetCursorPosition(int x, int y);

    void Clear();

    void Write(string text);

    Task<ConsoleKeyInfo> ReadKey(bool intercept = false);
}

public class ConsoleTerminal : ITerminal
{
    public int WindowWidth => Console.WindowWidth;
    public int WindowHeight => Console.WindowHeight;

    public ConsoleColor BackgroundColor
    {
        get => Console.BackgroundColor;
        set => Console.BackgroundColor = value;
    }

    public ConsoleColor ForegroundColor
    {
        get => Console.ForegroundColor;
        set => Console.ForegroundColor = value;
    }

    public void ResetColor() => Console.ResetColor();

    public void SetCursorPosition(int x, int y) => Console.SetCursorPosition(x, y);

    public void Clear() => Console.Clear();

    public void Write(string text) => Console.Write(text);

    public Task<ConsoleKeyInfo> ReadKey(bool intercept = false)
    {
        return Task.Run(() => Console.ReadKey(intercept));
    }
}
package com.axmor.calculator;

import org.robovm.apple.coregraphics.CGRect;
import org.robovm.apple.uikit.NSTextAlignment;
import org.robovm.apple.uikit.UIButton;
import org.robovm.apple.uikit.UIButtonType;
import org.robovm.apple.uikit.UIColor;
import org.robovm.apple.uikit.UIControl;
import org.robovm.apple.uikit.UIControlState;
import org.robovm.apple.uikit.UIEvent;
import org.robovm.apple.uikit.UIFont;
import org.robovm.apple.uikit.UITextView;
import org.robovm.apple.uikit.UIView;
import org.robovm.apple.uikit.UIViewController;

import scala.Tuple2;

public class MyViewController extends UIViewController {
	private static final String FONT = "Courier New";
	private static final String IT = "it";
	private static final String CLEAR = "C";
	private static final String DELETE = "←";
	private static final String EVALUATE = "E";
	
	private final UITextView console;
	private final UITextView input;
	private final UIFont consoleFont;
	private final UIFont inputFont;
	private final UIFont errorFont;
	
	private static final String[][] BUTTONS = {
		{ "7", "8", "9", "-", DELETE, EVALUATE },
		{ "4", "5", "6", "+", "*", "/" },
		{ "1", "2", "3", "(", ")", CLEAR },
	};
	
	private static final String[][] CHARS = {
		{ "q", "w", "e", "r", "t", "y", "u", "i", "o", "p" },
		{ "a", "s", "d", "f", "g", "h", "j", "k", "l", "=" },
		{ "z", "x", "c", "v", "b", "n", "m", ".", "^" }
	};

	private final int viewWidth;
	private final int viewHeight;
	private final int margin;
	private final int spacing;
	private final int mainRows;
	private final int charRows;
	private final int mainCols;
	private final int mainButtonSize;
	private final int charButtonSize;

	private boolean resultShown;

	private final Evaluator evaluator = new Evaluator(); // Stateful evaluator

	public MyViewController() {
		UIView view = getView();

		view.setBackgroundColor(UIColor.white());
		
		CGRect bounds = view.getBounds();

		viewWidth = (int) bounds.getWidth();
		viewHeight = (int) bounds.getHeight();
		margin = viewWidth / 20;
		spacing = viewWidth / 40;
		mainRows = BUTTONS.length;
		mainCols = BUTTONS[1].length;
		charRows = CHARS.length;
		mainButtonSize = (viewWidth - 2 * margin - (mainCols - 1) * spacing) / mainCols;
		charButtonSize = (mainButtonSize - spacing) / 2;

		int width = viewWidth - 2 * margin;
		int consoleAndInputHeight = viewHeight - mainRows * (mainButtonSize + spacing) - charRows
				* (charButtonSize + spacing) - 2 * margin - spacing;
		int consoleHeight = 3 * consoleAndInputHeight / 5;
		int inputHeight = 2 * consoleAndInputHeight / 5;
		int inputTop = margin + consoleHeight + spacing;
		int consoleFontSize = width / 25;
		int inputFontSize = width / 15;
		int errorFontSize = width / 25;
		consoleFont = UIFont.getFont(FONT, consoleFontSize);
		inputFont = UIFont.getFont(FONT, inputFontSize);
		errorFont = UIFont.getFont(FONT, errorFontSize);
		UIColor consoleColor = UIColor.fromRGBA(0.8, 0.8, 0.8, 1);

		console = new UITextView(new CGRect(margin, margin, width, consoleHeight));
		console.setFont(consoleFont);
		console.setTextAlignment(NSTextAlignment.Left);
		console.setBackgroundColor(consoleColor);
		console.setText(String.format("Type an expression and press 'E' to evaluate. Examples:%n%s%n%s%n%s",
				"2+sin(pi/2)", "x=2+2 (variable declaration)", "'C' clears the console and the variable table."));
		view.addSubview(console);

		input = new UITextView(new CGRect(margin, inputTop, width, inputHeight));
		input.setFont(UIFont.getFont(FONT, inputFontSize));
		input.setTextAlignment(NSTextAlignment.Left);
		view.addSubview(input);

		addButtons(view);
	}

	private void addButtons(UIView view) {
		int charButtonsTop = viewHeight - margin - charRows * (charButtonSize + spacing) + spacing;
		int buttonsTop = charButtonsTop - mainRows * (mainButtonSize + spacing);
		UIColor defaultColor = UIColor.fromRGBA(0.5, 0.5, 0.5, 1);
		UIColor evalColor = UIColor.fromRGBA(0.4, 0.6, 0.95, 1);
		UIColor clearColor = UIColor.fromRGBA(0.95, 0.4, 0.4, 1);
		for (int r = 0; r < mainRows; r++) {
			for (int c = 0; c < BUTTONS[r].length; c++) {
				int left = margin + c * (mainButtonSize + spacing);
				int top = buttonsTop + r * (mainButtonSize + spacing);
				String symbol = BUTTONS[r][c];
				UIColor color = symbol.equals(EVALUATE) ? evalColor
						: (symbol.equals(CLEAR) ? clearColor : defaultColor);
				addButton(view, left, top, mainButtonSize, mainButtonSize, color, symbol);
			}
		}

		addButton(view, margin, charButtonsTop, mainButtonSize, mainButtonSize, defaultColor, "0");
		addButton(view, margin, charButtonsTop + mainButtonSize + spacing, mainButtonSize, charButtonSize,
				defaultColor, IT);

		for (int r = 0; r < charRows; r++) {
			for (int c = 0; c < CHARS[r].length; c++) {
				int left = mainButtonSize + spacing + margin + c * (charButtonSize + spacing);
				int top = charButtonsTop + r * (charButtonSize + spacing);
				String symbol = CHARS[r][c];
				int buttonWidth = symbol.equals("it") ? 2 * charButtonSize + spacing : charButtonSize;
				addButton(view, left, top, buttonWidth, charButtonSize, defaultColor, symbol);
			}
		}
	}

	private void addButton(UIView view, int left, int top, int buttonWidth, int buttonHeight, UIColor color,
			String symbol) {
		UIButton button = UIButton.create(UIButtonType.RoundedRect);
		int buttonSize = Math.min(buttonWidth, buttonHeight);
		button.setFrame(new CGRect(left, top, buttonWidth, buttonHeight));
		button.setTitle(symbol, UIControlState.Normal);
		button.getTitleLabel().setFont(UIFont.getBoldSystemFont(buttonSize * 3 / 4));
		button.setBackgroundColor(color);
		button.setTintColor(UIColor.white());
		addButtonListener(button, symbol);
		view.addSubview(button);
	}

	private void addButtonListener(UIButton button, final String symbol) {
		button.addOnTouchUpInsideListener(new UIControl.OnTouchUpInsideListener() {
			@Override
			public void onTouchUpInside(UIControl control, UIEvent event) {
				onButtonPressed(symbol);
			}
		});
	}
	
	private void onButtonPressed(final String symbol) {
		if (resultShown) {
			resultShown = false;
			input.setText("");
			input.setFont(inputFont);
		}

		if (symbol.equals(EVALUATE)) {
			String expr = input.getText().trim();
			if (!expr.isEmpty()) {
				evaluate(expr);
			}
		} else if (symbol.equals(CLEAR)) {
			input.setText("");
			console.setText("");
			evaluator.reset();
		} else {
			input.setText(updateConsoleText(input.getText(), symbol));
		}
	}

	private void evaluate(String expr) {
		try {
			Tuple2<Object, String> resultTuple = evaluator.evaluate(expr);
			double value = resultTuple._1$mcD$sp();
			String result = resultToString(value);
			String variable = resultTuple._2();
			String consoleText = String.format("> %s%n%s = %s", expr, variable, result);

			if (console.getText().isEmpty()) {
				console.setText(consoleText);
			} else {
				console.setText(consoleText + String.format("%n%n") + console.getText());
			}

			input.setText(result);
		} catch (CalculatorException ex) {
			input.setFont(errorFont);
			input.setText(ex.getMessage());
		}
		resultShown = true;
	}

	private static String resultToString(double value) {
		return (value == (long) value) ? Long.toString((long) value) : Double.toString(value);
	}

	private static String updateConsoleText(String oldText, String symbol) {
		switch (symbol) {
			case DELETE:
				return oldText.isEmpty() ? oldText : oldText.substring(0, oldText.length() - 1);
			default:
				return oldText + symbol;
		}
	}
}

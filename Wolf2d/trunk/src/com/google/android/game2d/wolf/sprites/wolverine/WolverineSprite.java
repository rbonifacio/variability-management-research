package com.google.android.game2d.wolf.sprites.wolverine;

import android.graphics.Bitmap;

import com.google.android.game2d.api.LivelySprite;

public class WolverineSprite extends LivelySprite {
	private static final int COLUMNS = 9;
	private static final int ROWS = 4;
	
	private AbstractWolverineState state;

	public WolverineSprite(Bitmap bitmap, int initialColumn, int initialRow, int left, int row) {
		super(bitmap, COLUMNS, ROWS, initialColumn, initialRow, left, row);
		state = AbstractWolverineState.getRightState();
	}

	@Override
	public void moveDown() {
		// do nothing
	}

	@Override
	public void moveLeft() {
		state.moveLeft(this);
	}

	@Override
	public void moveRight() {
		state.moveRight(this);
	}

	@Override
	public void moveUp() {
		// do nothing
	}

	public void setState(AbstractWolverineState state) {
		this.state = state;
	}
	
}

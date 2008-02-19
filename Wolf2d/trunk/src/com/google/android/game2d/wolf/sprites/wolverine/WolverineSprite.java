package com.google.android.game2d.wolf.sprites.wolverine;

import android.graphics.Bitmap;

import com.google.android.game2d.api.LivelySprite;
import com.google.android.game2d.wolf.ImageEngine;

public class WolverineSprite extends LivelySprite {
	private static final int COLUMNS = 9;
	private static final int ROWS = 4;
	
	private AbstractWolverineState state;
	private static WolverineSprite instance;
	
	public static WolverineSprite instance() {
		if (instance == null) {
			Bitmap bm = ImageEngine.instance().getWolverineBitmap();
			instance = new WolverineSprite(bm, 0, 0, 20, 20);
		}
		return instance;
	}
	
	protected WolverineSprite(Bitmap bitmap, int initialColumn, int initialRow, int left, int row) {
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

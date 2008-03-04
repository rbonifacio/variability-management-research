package com.google.android.game2d.api.sprite;

import android.graphics.Bitmap;

public class FixedSprite extends AbstractSprite {

	public FixedSprite(Bitmap bitmap, int columns, int rows, int initialColumn, int initialRow, int left, int top) {
		super(bitmap, columns, rows, initialColumn, initialRow, left, top);
	}

	@Override
	public void moveDown() {
		// do nothing
	}

	@Override
	public void moveLeft() {
		// do nothing	
	}

	@Override
	public void moveRight() {
		// do nothing
	}

	@Override
	public void moveUp() {
		// do nothing
	}

}

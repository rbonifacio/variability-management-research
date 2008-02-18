package com.google.android.game2d.api;

import java.security.InvalidParameterException;

import android.graphics.Bitmap;

/**
 * A sprite that can change its frame or its position. 
 *   
 * @author rbonifacio
 *
 */
public abstract class LivelySprite extends AbstractSprite {
	protected Bitmap currentBitmap;

	/**
	 * @see AbstractSprite
	 */
	public LivelySprite(Bitmap bitmap, int columns, int rows, int initialColumn, int initialRow, int left, int row) {
		super(bitmap, columns, rows, initialColumn, initialRow, left, row);
	}

	/**
	 * @see AbstractSprite
	 */
	public LivelySprite(Bitmap bitmap, int columns, int rows) {
		super(bitmap, columns, rows);
	}

	/**
	 * Changes the current frame of the sprite
	 * @param column the column frame of the sprite (must be between 0 and columns)
	 * @param row the row frame of the sprite (must be between 0 and rows)
	 * @throws InvalidParameterException if column or row is out of bounds
	 */
	public void setFrame(int column, int row) throws InvalidParameterException {
		if (column >= 0 && column < columns && row >= 0 && row < rows) {
			this.currentColumn = column;
			this.currentRow = row;	
		}
		else {
			throw new InvalidParameterException("Column or row out of bounds");
		}
	}	
}

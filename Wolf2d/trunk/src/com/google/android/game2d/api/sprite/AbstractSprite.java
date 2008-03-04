package com.google.android.game2d.api.sprite;

import android.graphics.Bitmap;

/**
 * An abstract basic sprite. This class defines the common 
 * attributes for the different kinds of sprites (lively, fixed, 
 * and so on). A bitmap is used in order to create a sprite, and should 
 * be reused for different ones. 
 *   
 * @author rbonifacio
 *
 */
public abstract class AbstractSprite {
	protected Bitmap base;
	protected int currentRow;
	protected int currentColumn;
	protected int height;
	protected int width;
	protected int columns;
	protected int rows;
	protected int left;
	protected int top;
	
	/**
	 * Instantiate a sprite defining all relevant properties. 
	 * 
	 * @param bitmap a bitmap with frames that can be showed in a game scene
	 * @param the number of frames disposed at a row in the bitmap
	 * @param rows the number of rows disposed at a column in the bitmap
	 * @param initialColumn the column of the sprite initial frame
	 * @param initialRow the row of the sprite initial frame
	 */
	public AbstractSprite(Bitmap bitmap, int columns, int rows, int initialColumn, int initialRow, int left, int top) {
		this.base = bitmap;
		this.columns = columns;
		this.rows = rows;
		this.width = bitmap.width() / columns;
		this.height = bitmap.height() / rows;
		this.currentColumn = initialColumn;
		this.currentRow = initialRow;
		this.left = left;
		this.top = top;
	}
	
	/**
	 * Intantiate a sprite setting its initial column and row to 0.
	 * 
	 * @param bitmap a bitmap with frames that can be showed in a game scene
	 * @param the number of frames disposed at a row in the bitmap
	 * @param rows the number of rows disposed at a column in the bitmap
	 * @param rows
	 */
	public AbstractSprite(Bitmap bitmap, int columns, int rows) {
		this(bitmap, columns, rows, 0, 0, 0, 0);
	}

	/**
	 * Access method for the height property
	 * @return the height of each frame
	 */
	public int getHeight() {
		return height;
	}

	/**
	 * Access method for the width property
	 * @return the width of each frame
	 */
	public int getWidth() {
		return width;
	}

	/**
	 * Return the current bitmap for the sprite. 
	 * @return the current bitmap
	 */
	public Bitmap getBitmap() {
		int x = currentColumn*width;
		int y = currentRow*height;
		return Bitmap.createBitmap(base, x, y, width, height);
	}
	
	public abstract void moveLeft();
	public abstract void moveRight();
	public abstract void moveUp();
	public abstract void moveDown();

	public int getLeft() {
		return left;
	}

	public int getTop() {
		return top;
	}
	
	public void setLeft(int left) {
		this.left = left;
	}
	
	public void setTop(int top) {
		this.top = top;
	}

	public int getCurrentRow() {
		return currentRow;
	}

	public int getCurrentColumn() {
		return currentColumn;
	}
	
	
}

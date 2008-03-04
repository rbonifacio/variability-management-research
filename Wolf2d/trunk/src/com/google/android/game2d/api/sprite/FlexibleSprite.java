package com.google.android.game2d.api.sprite;

import android.graphics.Bitmap;

public class FlexibleSprite {

	protected Bitmap base;
	
	public FlexibleSprite(Bitmap base) {
		this.base = base;
	}
	
	public Bitmap getBitmap(int x, int y, int width, int height) {
		return Bitmap.createBitmap(base, x, y, width, height);
	}
}

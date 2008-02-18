package com.google.android.game2d.api.gui;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.drawable.BitmapDrawable;
import android.view.WindowManager;
import android.view.ViewGroup.LayoutParams;
import android.widget.ImageView;

public abstract class TiledView extends ImageView {
	public int DEFAULT_HEIGHT = 16;
	public int DEFAULT_WIDTH = 16;
	
	protected Bitmap base;
	protected Bitmap background;
	protected int map[][];
	protected int columns;
	protected int rows;
	int width;
	int height;
	
	public TiledView(Context context, Bitmap bitmap) {
		
		super(context);
		
		this.base = bitmap;
		WindowManager wm = (WindowManager)context.getSystemService("window");
		
		this.width = wm.getDefaultDisplay().getWidth();
		this.height = wm.getDefaultDisplay().getHeight();
		
		this.columns = this.width / DEFAULT_WIDTH;
		this.rows = height / DEFAULT_HEIGHT;
		
		map = new int[columns][rows];
		setBackgroundColor(Color.BLACK);
		defineTiles();
		defineBackground();
	}
	
	protected void defineBackground() {
		Bitmap bm = Bitmap.createBitmap(width, height, true);
		for(int i = 0; i < rows; i++) {
			for(int j = 0; j < columns; j++) {
				 int pixels[] = new int[16*16];
				 base.getPixels(pixels, 0, 16, map[j][i] * 16, 0, 16, 16);
				 bm.setPixels(pixels, 0, 16, j*16, i * 16, 16,16);
			}
		}
		setBackground(new BitmapDrawable(bm));
	}

	protected abstract void defineTiles();
}

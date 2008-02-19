package com.google.android.game2d.wolf;

import android.content.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

public class ImageEngine {
	
	private static ImageEngine instance;
	private Resources resources;
	private Bitmap wolverineBitmap;
	private Bitmap firstSceneBitmap;
	
	public static ImageEngine instance() {
		return instance;
	}
	
	static void initialize(Resources resources) {
		if (instance == null) {
			instance = new ImageEngine(resources);
		}
	}
	
	private ImageEngine(Resources resources) {
		this.resources = resources;
	}
	
	public Bitmap getWolverineBitmap() {
		if(wolverineBitmap == null) {
			wolverineBitmap = BitmapFactory.decodeResource(resources, R.drawable.wolverine);
		}
		return wolverineBitmap;
	}
	
	public Bitmap getFirstSceneBitmap() {
		if(firstSceneBitmap == null) {
			firstSceneBitmap = BitmapFactory.decodeResource(resources, R.drawable.bkg01);
		}
		return firstSceneBitmap;
	}
	

}

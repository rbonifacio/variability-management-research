package com.google.android.game2d.api.gui;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.widget.ImageView;

/**
 * Basic background for game scenes. It class does not 
 * need to be specialized, just instatiated with a background 
 * image. Then, this kind of view is responsible for rendering 
 * the background
 *   
 * @author rbonifacio
 */
public final class BackgroundView extends ImageView {
	
	private Bitmap background; 
	
	/**
	 * Create a new instance of BackgroundView
	 * @param context the application context
	 * @param background the background image used to render a game scene
	 */
	public BackgroundView(Context context, Bitmap background) {
		super(context);
		this.background = background;
		defineBackground();
	}
	
	private void defineBackground() {
		setBackground(new BitmapDrawable(background));
	}
}

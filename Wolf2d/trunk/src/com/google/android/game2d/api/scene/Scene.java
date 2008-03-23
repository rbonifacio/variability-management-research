package com.google.android.game2d.api.scene;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import android.graphics.Bitmap;

import com.google.android.game2d.api.sprite.AbstractSprite;

/**
 * This class represents a game scene. 
 * In Android Game API, just one scene 
 * will be activate at a given moment.  A scene is 
 * responsible for drawing its layers and sprites.
 * 
 * @author rbonifacio - rba2@cin.ufpe.br
 *
 */
public abstract class Scene {
	protected int width;
	protected int height;
	
	protected Bitmap[][] map;
	protected Bitmap background;
	protected List<AbstractSprite> sprites;
	protected List<Layer> layers;
	
	/**
	 * Basic constructor, just defining the 
	 * scene width and height 
	 * @param width scene width
	 * @param height scene height
	 */
	public Scene(int width, int height) {
		this.width = width;
		this.height = height;
		map = new Bitmap[width][height];
		sprites = new ArrayList<AbstractSprite>();
	}
	
	/**
	 * Constructor with the scene background, width, and 
	 * height
	 * 
	 * @param background scene background
	 * @param width scene width
	 * @param height scene height
	 */
	public Scene(Bitmap background, int width, int height) {
		this(width, height);
		this.background = background;
	}
	
	
	/**
	 * This method is executed in each game loop. It 
	 * should implement all updates required in the 
	 * game iterations (such as moving the main character, 
	 * update items and enemies, and so on). The developer must 
	 * implement this specific behavior for each scene. 
	 */
	public abstract void execute();
	
	public void draw() {
		for (Layer layer : layers) {
			if(layer.isVisible()) {
				layer.draw();
			}
		}
	}
	
	public Bitmap getTile(int x, int y) {
		return map[x][y]; 
	}

	public Bitmap getBackground() {
		return background;
	}

	public void setBackground(Bitmap background) {
		this.background = background;
	}

	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}
	
	public void addSprite(AbstractSprite sprite) {
		sprites.add(sprite);
	}
	
	public AbstractSprite[] sprites() {
		AbstractSprite[] array = new AbstractSprite[sprites.size()];
		return sprites.toArray(array);
	}
	
	
}
